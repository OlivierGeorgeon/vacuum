import java.util.ArrayList;


public class Action {

	public String name;
	public int width;                // plage of selection
	public int height;               // plage of input
	public ArrayList<float[][]> selectMap;
	public ArrayList<float[][]> confidenceMap;
	public float scale;
	
	public int previousInput=0;
	public int previousChoice=0;
	public int previousObj=0;
	private int[] candidates;
	
	public Action(String n,float s,int w,int h){
		
		name=n;
		width=w;
		height=h;
		scale=s;
		
		selectMap=new ArrayList<float[][]>();
		confidenceMap=new ArrayList<float[][]>();
		
		/*
		selectMap.add(new float[width][height]);
		selectMap.add(new float[width][height]);
		
		confidenceMap.add(new float[width][height]);
		confidenceMap.add(new float[width][height]);
		
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(selectMap.get(0))[i][j]=10;
				(selectMap.get(1))[i][j]=10;
				
				(confidenceMap.get(0))[i][j]=0;
				(confidenceMap.get(1))[i][j]=0;
			}
		}
		*/
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
				(confidenceMap.get(index))[i][j]=0;
			}
		}
		return index;
	}
	
	// select a distance step value from the most probable ones
	public float selectOutput(float input,int obj){
		
		int indexObj=obj;
		int d=(int) Math.min(input, height-1);
		// save the actual input and object number
		previousInput=d;
		previousObj=obj;
		
		// set weights
		int count=0;
		for (int i=0;i<width;i++){
			candidates[i]=(int) selectMap.get(indexObj)[i][d]+100;
			count+=selectMap.get(indexObj)[i][d]+100;
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
		//if (previousObj==1) System.out.println( "<<<<<<<<<<<<<<<<<<<<<< "+ previousChoice + " , " +previousInput+ " , "+ (100- ( (previousChoice-previousInput)*(previousChoice-previousInput) ) ) + " , "+reward+" >>>>>>>>>>>>>>>>>>>>>>>>>>");
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
				if (!out && d<=15){
					if (d<=1){
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+ (float)1);
						selectMap.get(previousObj)[i2][j2]=( selectMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    + r ) / newConfidence;
					}
					else{
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+(1.0/d));
						selectMap.get(previousObj)[i2][j2]=( selectMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    +(r/(float)d) ) / newConfidence;
					}
					confidenceMap.get(previousObj)[i2][j2]=newConfidence;
					
					selectMap.get(previousObj)[i2][j2]=minmax(selectMap.get(previousObj)[i2][j2]);
				}
			}
		}
	}
	
	private float minmax(float a){
		if      (a<-100) return -100;
		else if (a> 100) return  100;
		else             return  a;
	}
	
}
