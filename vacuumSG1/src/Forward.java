import java.util.ArrayList;


public class Forward {

	public int width;                // plage of selection
	public int height;               // plage of distance
	public ArrayList<float[][]> forwardMap;
	public ArrayList<float[][]> confidenceMap;
	public float scale;
	
	public int previousDistance=0;
	public int previousChoice=0;
	public int previousObj=0;
	//private ArrayList<Integer> candidates;
	private int[] candidates;
	
	public Forward(int s){
		width=100;
		height=160;
		scale=s;
		
		forwardMap=new ArrayList<float[][]>();
		confidenceMap=new ArrayList<float[][]>();
		
		forwardMap.add(new float[width][height]);
		forwardMap.add(new float[width][height]);
		
		confidenceMap.add(new float[width][height]);
		confidenceMap.add(new float[width][height]);
		
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(forwardMap.get(0))[i][j]=10;
				(forwardMap.get(1))[i][j]=10;
				//forwardMap[i][j]=Math.max((float) (100-  3*Math.sqrt( (50-i)*(50-i) + (50-j)*(50-j))) , -100);
				
				(confidenceMap.get(0))[i][j]=0;
				(confidenceMap.get(1))[i][j]=0;
			}
		}
		
		//candidates = new ArrayList<Integer>();
		candidates=new int[width];
	}
	
	// add new matrix
	public int addObject(){
		int index=forwardMap.size();
		forwardMap.add(new float[width][height]);
		confidenceMap.add(new float[width][height]);
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(forwardMap.get(index))[i][j]=10;
				(confidenceMap.get(index))[i][j]=10;
			}
		}
		return index;
	}
	
	// select a distance step value from the most probable ones
	public float selectDistance(float distance,int obj){
		
		int indexObj=obj;
		int max=-100;
		int d=(int) Math.min(distance, height-1);
		previousDistance=(int) Math.min((int)d,height-1);
		previousObj=obj;
		// find the maximum
		/*
		for (int i=0;i<width;i++){
			if ( forwardMap.get(indexObj)[i][d] >max) max=(int) forwardMap.get(indexObj)[i][d];
		}
		candidates.clear();
		for (int i=0;i<width;i++){
			if (forwardMap.get(indexObj)[i][d] >max-20) candidates.add(i);
		}
		int index= (int) (Math.random()*(double)candidates.size());
		previousChoice= candidates.get(index);
		return (float)candidates.get(index)/scale/2;
		*/
		
		int count=0;
		for (int i=0;i<width;i++){
			candidates[i]=(int) forwardMap.get(indexObj)[i][d]+100;
			count+=forwardMap.get(indexObj)[i][d]+100;
		}
		int rand=(int) (Math.random()*count);
		
		int i=0;
		while (count>rand && i<width){
			count-=candidates[i];
			i++;
		}
		previousChoice= i;
		
		return (float)i/scale/2;
		
	}
	
	
	
	// apply results (success or fail) to the previous decision
	public void setResults(float reward){
		
		for (int i=-30;i<30;i++){
			for (int j=-30;j<30;j++){
				
				double d=Math.sqrt(i*i+j*j)/2;
				boolean out=false;
				
				float newConfidence;
				
				int i2=previousChoice+i;
				if (i2>=width || i2< 0) out=true;
				int j2=previousDistance+j;
				if (j2>=height || j2< 0) out=true;
				
				if (!out && d<=15){
					//if (d>=1) forwardMap.get(previousObj)[i2][j2] += (reward-forwardMap.get(previousObj)[i2][j2])/(5*d);
					//else      forwardMap.get(previousObj)[i2][j2] += (reward-forwardMap.get(previousObj)[i2][j2])/ 5;
					if (d<=1){
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+ 1.0);
						forwardMap.get(previousObj)[i2][j2]=( forwardMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    + reward ) / newConfidence;
					}
					else{
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+(1.0/d));
						forwardMap.get(previousObj)[i2][j2]=( forwardMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    +(reward/(float)d) ) / newConfidence;
					}
					confidenceMap.get(previousObj)[i2][j2]=newConfidence;
					
					forwardMap.get(previousObj)[i2][j2]=minmax(forwardMap.get(previousObj)[i2][j2]);
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
