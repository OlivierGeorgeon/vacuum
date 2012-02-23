package agent;

import javax.vecmath.Vector3f;

public class InnerEar {

	public double left_x=0;
	public double left_y=0;
	public double right_x=0;
	public double right_y=0;
	
	public int[][] leftMap;
	public int[][] rightMap;
	
	public InnerEar(){
		leftMap=new int[200][200];
		rightMap=new int[200][200];
		
		for (int i=0;i<200;i++){
			for (int j=0;j<200;j++){
				leftMap[i][j]=0;
				rightMap[i][j]=0;
			}
		}
	}
	
	public void computeEars(Vector3f egoSpeedT, Vector3f SpeedR){
		left_x=egoSpeedT.x - 0.5*SpeedR.z;
		right_x=egoSpeedT.x+ 0.5*SpeedR.z;
		
		left_y=egoSpeedT.y;
		right_y=egoSpeedT.y;
		
		/*
		for (int i=0;i<200;i++){
			for (int j=0;j<200;j++){
				if (leftMap[i][j]>0) leftMap[i][j]--;

			}
		}*/
		
		if (Math.abs(left_x*500)<100 && Math.abs(left_y*500)<100){
			leftMap[(int)(left_x*500)+100][(int)(left_y*500)+100]=10;
		}
		
		if (Math.abs(right_x*500)<100 && Math.abs(right_y*500)<100){
			rightMap[(int)(right_x*500)+100][(int)(right_y*500)+100]=10;
		}
		
		//System.out.println("++++++++++++++++++ "+left_x+" , "+right_x);
	}
}
