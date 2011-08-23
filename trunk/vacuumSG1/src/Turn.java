import java.util.ArrayList;


public class Turn {
	
	public int width;                // plage of selection
	public int height;               // plage of angle
	public ArrayList<float[][]> turnMap;
	public ArrayList<float[][]> confidenceMap;
	public float scale;
	
	public int previousAngle=0;
	public int previousChoice=0;
	public int previousObj=0;
	private int[] candidates;
	
	public Turn(int s){
		width=180;
		height=180;
		scale=s;
		
		turnMap=new ArrayList<float[][]>();
		
		turnMap.add(new float[width][height]);
		turnMap.add(new float[width][height]);
		
		confidenceMap.add(new float[width][height]);
		confidenceMap.add(new float[width][height]);
		
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(turnMap.get(0))[i][j]=10;
				(turnMap.get(1))[i][j]=10;
				
				(confidenceMap.get(0))[i][j]=0;
				(confidenceMap.get(1))[i][j]=0;
			}
		}
		candidates = new int[width];
	}
		
	// add new matrix
	public int addObject(){
		int index=turnMap.size();
		turnMap.add(new float[width][height]);
		for (int i=0;i<width;i++){
			for (int j=0;j<height;j++){
				(turnMap.get(index))[i][j]=10;
			}
		}
		return index;
	}
	
	// select a distance step value from the most probable ones
	public float selectDistance(float distance,int obj){
		
		int indexObj=obj;
		int max=-100;
		int d=(int) Math.min(distance, height-1);
		previousAngle=(int) Math.min((int)d,height-1);
		previousObj=obj;
		
		int count=0;
		for (int i=0;i<width;i++){
			candidates[i]=(int) turnMap.get(indexObj)[i][d]+100;
			count+=turnMap.get(indexObj)[i][d]+100;
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
				int j2=previousAngle+j;
				if (j2>=height || j2< 0) out=true;
				
				if (!out && d<=15){
					if (d<=1){
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+ 1.0);
						turnMap.get(previousObj)[i2][j2]=( turnMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    + reward ) / newConfidence;
					}
					else{
						newConfidence=(float) (confidenceMap.get(previousObj)[i2][j2]+(1.0/d));
						turnMap.get(previousObj)[i2][j2]=( turnMap.get(previousObj)[i2][j2]*confidenceMap.get(previousObj)[i2][j2]
						                                    +(reward/(float)d) ) / newConfidence;
					}
					confidenceMap.get(previousObj)[i2][j2]=newConfidence;
					
					turnMap.get(previousObj)[i2][j2]=minmax(turnMap.get(previousObj)[i2][j2]);
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
