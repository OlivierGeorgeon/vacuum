import java.awt.Color;
import java.util.ArrayList;



public class VisualMap {

	public Color[][] colorMap;
	public float[][] potentialMap;
	public float[][] confidenceMap;
	public ErnestModel ernest;
	public int mapSize,mapSizeTheta,mapSizeR;
	
	public VisualMap(ErnestModel e){
		ernest=e;
		mapSize=50;
		mapSizeTheta=180;
		mapSizeR=100;
		colorMap=new Color[mapSizeTheta][mapSizeR];
		potentialMap=new float[mapSize][mapSize];
		confidenceMap=new float[mapSizeTheta][mapSizeR];
		
		for (int i=0;i<180;i++){
			for (int j=0;j<mapSizeR;j++){
				confidenceMap[i][j]=-1;
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMap[i][j]=0;
			}
		}
		
	}
	
	
	public void seeEnvironement(double[] r,Color[] c){
		
		
		// set colors on polar map
		for (int i=0;i<180;i++){
			
			int confidence=Math.min(40, Math.abs(i-90)/2+1);
			int min=(int) r[i]-1;//(int) Math.max(0, r[i]-confidence/2);
			int max=(int) r[i]+5;//(int) Math.min(mapSizeR, r[i]+confidence/2+1);

			for (int j=0;j<mapSizeR;j++){

				if (j>=min && j<max){
					colorMap[i][j]=c[i];
					confidenceMap[i][j]=5;
				}
				else{
					colorMap[i][j]=null;
					confidenceMap[i][j]=-1;
				}
				
			}
		}
		
		// generate Cartesian potential map
		float x,y;
		double sum;
		double counter;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				x= (float) Math.sqrt( (float)((i-25)*(i-25) + (j-25)*(j-25)) );
				if (x>0 && j-25<=0){
					y=0;
					if (j-25 <0) y=(float)( Math.atan( -((float)i-25)/((float)j-25) )*180/Math.PI);
					else if (j-25==0 && i-25>0) y= 90;
					else if (j-25==0 && i-25<0) y=-90;
					
					int x2=Math.round(x*2);
					int y2=Math.round(y+90);
					
					if (y2>=180) y2=179;
					
					counter=0;
					sum=0;
					for (int i2=-4;i2<=4;i2++){
						for (int j2=-4;j2<=4;j2++){
							if (x2+i2>=0 && x2+i2<mapSizeR && y2+j2>=0 && y2+j2<mapSizeTheta){
								if (confidenceMap[y2+j2][x2+i2]>=0){
									sum+=1;
									counter+=1;
								}
								else{
									counter+=1;
								}
							}
						}
					}
					
					potentialMap[i][j]=(float) (sum/counter);
					
					/*if (confidenceMap[y2][x2]>=0) 
						potentialMap[i][j]=1;
					else{
						potentialMap[i][j]=0;
					}*/
				}
			}
		}

	}
	
	
}