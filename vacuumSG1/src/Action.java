import java.awt.Color;
import java.util.ArrayList;


public class Action {

	public String name;
	public int width;                // plage of selection
	public int height;               // plage of input
	public ArrayList<float[][]> selectMap;
	public ArrayList<float[][]> confidenceMap;
	public ObjectMemory objMemory;
	public float scale;
	public int sampling=5;
	public ArrayList<ArrayList<Float>> distances;
	
	
	public int previousInput=0;
	public int previousChoice=0;
	public int previousObj=0;
	private int[] candidates;
	private Color previousObjColor;
	
	public float Icoef=(float) 0.5;  // initial coefficients of links
	
	public Action(String n,float s,int w,int h, ObjectMemory m){
		
		name=n;
		width=w;
		height=h;
		scale=s;
		
		selectMap=new ArrayList<float[][]>();
		confidenceMap=new ArrayList<float[][]>();
		distances=new ArrayList<ArrayList<Float>>();
		
		objMemory=m;
		
		//candidates = new ArrayList<Integer>();
		candidates=new int[width];
	}
	
	
	// add new matrix
	// a selectMap is the matrix of values
	// a confidenceMap give the reliability of each value
	public int addObject(){
		int index=selectMap.size();
		selectMap.add(new float[width/sampling][height/sampling]);
		confidenceMap.add(new float[width/sampling][height/sampling]);
		for (int i=0;i<width/sampling;i++){
			for (int j=0;j<height/sampling;j++){
				(selectMap.get(index))[i][j]=10;
				(confidenceMap.get(index))[i][j]=0;
			}
		}
		
		return index;
	}
	
	
	// select a distance step value from the most probable ones
	public float selectOutput(float input,Color rgb){
		
		// add matrix if new objects were created
		while (objMemory.objectList.size()>selectMap.size()){
			int index=selectMap.size();
		
			
			selectMap.add(new float[width/sampling][height/sampling]);
			confidenceMap.add(new float[width/sampling][height/sampling]);
			for (int i=0;i<width/sampling;i++){
				for (int j=0;j<height/sampling;j++){
					(selectMap.get(index))[i][j]=10;
					(confidenceMap.get(index))[i][j]=0;
				}
			}
			
			for (int i=0;i<index;i++){
				distances.get(i).add(distance( objMemory , objMemory.objectList.get(index) , objMemory.objectList.get(i)));
			}
			
			distances.add(new ArrayList<Float>());
			
			for (int i=0;i<index+1;i++){
				distances.get(index).add(distance( objMemory , objMemory.objectList.get(index) , objMemory.objectList.get(i)));
			}
			
		}
		
		int obj=objMemory.indexOfColor(rgb);
		
		int indexObj=obj;
		int d=(int) Math.min(input, height-sampling-1);
		// save the actual input and object number
		previousInput=d;
		previousObj=obj;
		previousObjColor=rgb;
		
		// set weights
		int count=0;
		for (int i=0;i<width-sampling;i++){
			
			if ( i == (int)(i/sampling)*sampling && d == (int)(d/sampling)*sampling){
				candidates[i]=(int) (selectMap.get(indexObj)[i/sampling][d/sampling]+100);
				count+=selectMap.get(indexObj)[i/sampling][d/sampling]+100;
			}
			else{
				float x11= selectMap.get(indexObj)[i/sampling  ][d/sampling]+100;
				float x12= selectMap.get(indexObj)[i/sampling+1][d/sampling]+100;
				
				float x21= selectMap.get(indexObj)[i/sampling  ][d/sampling+1]+100;
				float x22= selectMap.get(indexObj)[i/sampling+1][d/sampling+1]+100;
				
				float x1 = x11*(1- (i-(int)(i/sampling)*sampling)/sampling) 
						 + x12*(((int)(i/sampling+1)*sampling-i)/sampling);
				float x2 = x21*(1- (i-(int)(i/sampling)*sampling)/sampling) 
				 	   	 + x22*(((int)(i/sampling+1)*sampling-i)/sampling);
				
				float x = x1*(1- (d-(int)(d/sampling)*sampling)/sampling) 
						+ x2*(((int)(d/sampling+1)*sampling-d)/sampling);
				
				candidates[i]=(int) x+1;
				count+=(int)x+1;
				
			}
			
		}
		int rand=(int) (Math.random()*count);
		
		// select a value
		int i=0;
		while (count>rand && i<width){
			count-=candidates[i];
			i++;
		}
		previousChoice= i;

		return (float)i/scale;
		
	}
	
	
	
	// apply results (success or fail) to the previous decision
	public void setResults(float reward){
		
		float r=minmax(reward);
		
		for (int k=0;k<selectMap.size();k++){
		
			double d2;
			if (k!=previousObj){
				d2=distances.get(previousObj).get(k)*5;
				if (d2<=1.1) d2=1.1;
			}
			else  d2=0;
			
			if (d2<=1000){
			
				for (int i=0;i<width/sampling;i++){
					for (int j=0;j<height/sampling;j++){
				
						double d= Math.sqrt((previousChoice-i*sampling)*(previousChoice-i*sampling)
								   +(previousInput -j*sampling)*(previousInput -j*sampling) );
				
						float newConfidence;
				
						// set new probability an confidence values
						if (d<=40){
							if (d<=1 && d2==0){
								newConfidence=(float) (confidenceMap.get(k)[i][j]+ (float)1);
								selectMap.get(k)[i][j]=( selectMap.get(k)[i][j]*confidenceMap.get(k)[i][j]
						                                    + r ) / newConfidence;
								confidenceMap.get(k)[i][j]=newConfidence;
							}
							else if (d2==0){
								newConfidence=(float) (confidenceMap.get(k)[i][j]+(1.0/d));
								selectMap.get(k)[i][j]=( selectMap.get(k)[i][j]*confidenceMap.get(k)[i][j]
						                                    +(r/(float)d) ) / newConfidence;
								confidenceMap.get(k)[i][j]=newConfidence;
							}
							else{
								newConfidence=(float) (confidenceMap.get(k)[i][j]+(0.1/(d+d2)));
								selectMap.get(k)[i][j]=( selectMap.get(k)[i][j]*confidenceMap.get(k)[i][j]
						                                    +( (float)0.1*r/(float)(d+d2)) ) / newConfidence;
								
								confidenceMap.get(k)[i][j]=newConfidence;
							}
					
					
							selectMap.get(k)[i][j]=minmax(selectMap.get(k)[i][j]);
						}
					}
				}
			}
		}
		distance(objMemory);
		
	}
	
	
	public void distance(ObjectMemory objMem){
		for (int i=0;i<distances.size();i++){
			distances.get(objMemory.indexOfColor(previousObjColor)).set(i,distance(objMemory,previousObjColor,objMemory.objectList.get(i)));
			distances.get(i).set(objMemory.indexOfColor(previousObjColor),distance(objMemory,previousObjColor,objMemory.objectList.get(i)));
		}
	}
	
	public float distance(ObjectMemory objMem,Color c1,Color c2){
		
		int index1=objMem.indexOfColor(c1);
		int index2=objMem.indexOfColor(c2);
		
		float distance=0;
		
		int r1=c1.getRed();
		int g1=c1.getGreen();
		int b1=c1.getBlue();
		
		int r2=c2.getRed();
		int g2=c2.getGreen();
		int b2=c2.getBlue();
		
		float count=0;
		
		for (int i=0;i<width/sampling;i++){
			for (int j=0;j<height/sampling;j++){
				
				float cmin=Math.min(confidenceMap.get(index1)[i][j]+1,confidenceMap.get(index2)[i][j]+1);
				
				if (cmin>1){
					float colorDistance=(float)(Math.sqrt( (float)((r1-r2)*(r1-r2) + (g1-g2)*(g1-g2) + (b1-b2)*(b1-b2)) ));
					float mapDistance=2*Math.abs( selectMap.get(index1)[i][j]-selectMap.get(index2)[i][j] );
					distance+= mapDistance*mapDistance*(1 - (1/(cmin*cmin)) )  +  colorDistance*(0.1/cmin);
					count+= mapDistance*(1-(1/(cmin*cmin))) + (0.1/cmin);
				}
				else{
					float colorDistance=(float)(Math.sqrt( (float)((r1-r2)*(r1-r2) + (g1-g2)*(g1-g2) + (b1-b2)*(b1-b2)) ));
					distance+= colorDistance*0.1;
					count+=0.1;
				}
			}
		}
		
		return distance/count;
	}
	
	
	private float minmax(float a){
		if      (a<-100) return -100;
		else if (a> 100) return  100;
		else             return  a;
	}
	
}
