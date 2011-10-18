import java.awt.Color;
import java.util.ArrayList;




public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	public float[][] worldMap;
	
	public Colliculus(TactileMap t,VisualMap v){
		tmap=t;
		vmap=v;

		worldMap =new float[50][50];
	}
	
	public void update(double[] r,Color[] c,double[] rm,Color[] cm,int[] corners,int action,float speed,double[] r3){
		/*
		for (int i=0;i<50;i++){
			for (int j=0;j<50;j++){
				worldMap[i][j]=0;
			}
		}
		
		for (int i=0;i<360;i++){
			int x= (int) (r3[i]*Math.cos(i*Math.PI/180));
			int y= (int) (r3[i]*Math.sin(i*Math.PI/180));
			
			if (x>=0 && x<50 && y>=0 && y<50){
				worldMap[x][y]=1;
			}
		}*/
		
		tmap.touchEnvironment(r, c, action, speed);
		//vmap.seeEnvironment(rm, cm,corners, action, speed);
	}

}