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
	
	public ArrayList<ArrayList<Float>> links; // links between objects an matrix
	
	public int previousInput=0;
	public int previousChoice=0;
	public Color previousObjColor;
	private int[] candidates;
	
	public float Icoef=(float) 0.2;  // initial coefficients of links
	
	public Action(String n,float s,int w,int h, ObjectMemory m){
		
		name=n;
		width=w;
		height=h;
		scale=s;
		
		selectMap=new ArrayList<float[][]>();
		confidenceMap=new ArrayList<float[][]>();
		
		objMemory=m;
		
		links=new ArrayList<ArrayList<Float>>();
		int nb=objMemory.objectList.size();
		for (int i=0;i<nb;i++){
			links.add(new ArrayList<Float>());
		}
		
		//candidates = new ArrayList<Integer>();
		candidates=new int[width];
	}
	
	
	// add new matrix
	// a selectMap is the matrix of values
	// a confidenceMap give the reliability of each value
	public int addObject(){
		int index=selectMap.size();
		selectMap.add(new float[width][height]);
		confidenceMap.add(new float[width][height]);
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(selectMap.get(index))[i][j]=10;
				(confidenceMap.get(index))[i][j]=(float) 0.01;
			}
		}
		
		// connect every object to the new matrix
		int nb=links.size();
		for (int i=0;i<nb;i++){
			links.get(i).add(Icoef);
		}
		
		return index;
	}
	
	
	// change the link between an object ant a matrix
	public void setLink(int indexObj,int indexMat,float val){
		// add new links if a new object was created
		while (links.size() < objMemory.objectList.size()){
			links.add(new ArrayList<Float>() );
			int nb=selectMap.size();
			for (int i=0;i<nb;i++){
				links.get(links.size()-1).add(Icoef);
			}
		}
		links.get(indexObj).set(indexMat, val);
	}
	
	
	public void setLink(Color rgb,int indexMat,float val){
		// add new links if a new object was created
		while (links.size() < objMemory.objectList.size()){
			links.add(new ArrayList<Float>() );
			int nb=selectMap.size();
			for (int i=0;i<nb;i++){
				links.get(links.size()-1).add(Icoef);
			}
		}
		links.get(objMemory.objectList.indexOf(rgb)).set(indexMat, val);
	}
	
	
	// select a distance step value from the most probable ones
	public float selectOutput(float input,Color rgb){
		
		// add new links if a new object was created
		while (links.size() < objMemory.objectList.size()){
			links.add(new ArrayList<Float>() );
			int nb=selectMap.size();
			for (int i=0;i<nb;i++){
				links.get(links.size()-1).add(Icoef);
			}
		}
		
		int obj=objMemory.indexOfColor(rgb);		

		// create new matrix if there is the object is not connected
		// and connect this object with a weight of 1
		if (!isConnected(rgb)){
			this.addObject();
			setLink(rgb,selectMap.size()-1,1);
		}
		
		

		
		int indexObj=obj;
		int d=(int) Math.min(input, height-1);
		
		// save the actual input and object color
		previousInput=d;
		previousObjColor=rgb;
		
		// set weights on the selection vector
		int count=0;
		for (int i=0;i<width;i++){
			float weight=0;
			candidates[i]=0;
			for (int j=0;j<selectMap.size();j++){
				candidates[i]+= (int)( (selectMap.get(j)[i][d]+100)*links.get(indexObj).get(j)*10 );
				weight+=links.get(indexObj).get(j)*10;
			}
			candidates[i]=(int) ((float)candidates[i]/weight);
			count+=((float)candidates[i]);
			
			//candidates[i]=(int) selectMap.get(indexObj)[i][d]+100;
			//count+=selectMap.get(indexObj)[i][d]+100;
		}
		int rand=(int) (Math.random()*(count));
		
		// select a value
		int i=0;
		while (count>rand && i<width){
			count-=candidates[i];
			i++;
		}
		previousChoice= i-1;
		if (i<0) i=0;
		return (float)i/scale;
	}
	
	
	
	// apply results (success or fail) to the previous decision
	public void setResults(float reward){
		
		int indexObj=objMemory.indexOfColor(previousObjColor);
		float r=minmax(reward);
		int nbMatrix=selectMap.size();
		float error=0;					// error between given and real value
		float oldConfidence=0;
		
		if (this.name.equals("forward"))
		
		for (int k=0;k<nbMatrix;k++){
			
			//System.out.println("test "+previousChoice+" "+previousInput);
			error=Math.abs(selectMap.get(k)[previousChoice][previousInput]-r);
			oldConfidence=confidenceMap.get(k)[previousChoice][previousInput];
			
			float weight=links.get(indexObj).get(k);
		
			for (int i=-30;i<30;i++){
				for (int j=-30;j<30;j++){
					
					double d=Math.sqrt(i*i+j*j)/2;
					boolean out=false;
					
					float newConfidence;
				
					int i2=previousChoice+i;
					if (i2>=width || i2< 0) out=true;
					int j2=previousInput+j;
					if (j2>=height || j2< 0) out=true;
				
					// set new probability an confidence values
					if (!out && d<=15 && weight>0.5){
						
						if (d<=1){
							
							newConfidence=(float) (confidenceMap.get(k)[i2][j2]+ weight*weight);
							selectMap.get(k)[i2][j2]=( selectMap.get(k)[i2][j2]*confidenceMap.get(k)[i2][j2]
							                         + r*weight*weight ) / newConfidence;
						}
						else{
							newConfidence=(float) (confidenceMap.get(k)[i2][j2]+ weight*weight/(float)d);
							selectMap.get(k)[i2][j2]=( selectMap.get(k)[i2][j2]*confidenceMap.get(k)[i2][j2]
							                         + r*weight*weight/(float)d ) / newConfidence;
						}
						confidenceMap.get(k)[i2][j2]=newConfidence;			
						selectMap.get(k)[i2][j2]=minmax(selectMap.get(k)[i2][j2]);
					}
				}
			}
			
			// correction of the weight of links
			
			// a is the "limit of acceptable error"
			float a= 200/(oldConfidence+1);
			
			// b is the maximum modification
			float b= (1-weight)*(oldConfidence*oldConfidence+1)/200;
			
			float modif = (error-a)*(-oldConfidence-1)/2;
			modif= Math.min( b , Math.max(-b , modif));
			
			links.get(indexObj).set(k, Math.min(1, Math.max(0, links.get(indexObj).get(k)+modif) ) );
			
			if (this.name.equals("forward"))
			System.out.print(" , "+links.get(indexObj).get(k));
			
		}
		System.out.println();
	}
	
	
	
	public boolean isConnected(Color rgb){
		int index=objMemory.indexOfColor(rgb);
		boolean connected=false;
		if (index!=-1){
			for (int i=0;i<selectMap.size();i++){
				if (links.get(index).get(i) >0.1) connected=true;
			}
		}
		return connected;
	}
	
	
	private float minmax(float a){
		if      (a<-100) return -100;
		else if (a> 100) return  100;
		else             return  a;
	}
	
}
