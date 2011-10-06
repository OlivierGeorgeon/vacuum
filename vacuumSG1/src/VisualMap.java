import java.awt.Color;
import java.util.ArrayList;



public class VisualMap {

	public Color[][] colorMap;
	public float[][] confidenceMap;
	public ErnestModel ernest;
	public int mapSizeTheta,mapSizeR;
	
	public VisualMap(ErnestModel e){
		ernest=e;
		mapSizeTheta=180;
		mapSizeR=100;
		colorMap=new Color[mapSizeTheta][mapSizeR];
		confidenceMap=new float[mapSizeTheta][mapSizeR];
		
		for (int i=0;i<180;i++){
			for (int j=0;j<mapSizeR;j++){
				confidenceMap[i][j]=-1;
			}
		}
		
	}
	
	
	public void seeEnvironement(double[] r,Color[] c){
		
		for (int i=0;i<180;i++){
			
			int confidence=Math.min(40, Math.abs(i-90)/2+1);
			int min=(int) Math.max(0, r[i]-confidence/2);
			int max=(int) Math.min(mapSizeR, r[i]+confidence/2+1);
			
			
			for (int j=0;j<mapSizeR;j++){

				if (j>=min && j<max){
					colorMap[i][j]=c[i];
					confidenceMap[i][j]=confidence;
				}
				else{
					colorMap[i][j]=null;
					confidenceMap[i][j]=-1;
				}
			}
		}
		
	}
	
	
}