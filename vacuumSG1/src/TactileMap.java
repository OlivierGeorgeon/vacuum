import java.awt.Color;


public class TactileMap {

	public float[] m_tactilePressure;
	public float[] m_tactilePressureOld;
	public float[] m_tactileVariations;
	public double[][] connections;
	public double[][] confidence;
	public double angle;
	public Color[] m_tactileObject;
	public double[] sensorX,sensorX2;
	public double[] sensorY,sensorY2;
	public double attraction,repulsion;
	public ErnestModel ernest;
	public int resolution;
	
	public float chargeMap[][];
	public float potentialMap[][];
	public int mapSize;
	
	public TactileMap(ErnestModel e){
		resolution=18;
		mapSize=50;
		m_tactilePressure=new float[resolution];
		m_tactilePressureOld=new float[resolution];
		m_tactileVariations=new float[resolution];
		m_tactileObject=new Color[resolution];
		connections=new double[resolution][resolution];
		confidence =new double[resolution][resolution];
		sensorX=new double[resolution];
		sensorY=new double[resolution];
		attraction=0.001;
		ernest=e;
		
		chargeMap=new float[mapSize][mapSize];
		potentialMap=new float[mapSize][mapSize];
		
		double d;
		for (int i=0;i<resolution;i++){
			m_tactilePressure[i]=0;
			m_tactilePressureOld[i]=0;
			m_tactileVariations[i]=0;
			for (int j=0;j<resolution;j++){
				connections[i][j]=0;
				confidence[i][j]=0;
			}
			
			// initialize neurons positions
			sensorX[i]= (float) (Math.random()*200-100);//-50*Math.sin(360/resolution*i*Math.PI/180);
			sensorY[i]= (float) (Math.random()*200-100);// 50*Math.cos(360/resolution*i*Math.PI/180);
		}
		
		normalize();
		
		for (int i=0;i<25;i++){
			for (int j=0;j<25;j++){
				chargeMap[i][j]=0;
				potentialMap[i][j]=0;
			}
		}
	}
	
	
	
	public void touchEnvironment(double[] r,Color[] c){
		float distance,distance2;
		int angle=360/resolution;
		
		// sensors around ernest
		for (int i=0;i<resolution;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
		}
		int E_angle=ernest.m_orientation+540;
		for (int i=0;i<360;i+=angle){
			distance=(float) r[(i+E_angle)%360];
			distance2=1- distance/10;
			if (distance<=10){
				m_tactilePressure[i/angle]= distance2;
				m_tactileObject[i/angle]=c[(i+E_angle)%360];
			}
			else{
				m_tactilePressure[i/angle]=0;
				m_tactileObject[i/angle]=Color.black;
			}
		}
		
		
		// sensors in front of ernest
		/*for (int i=0;i<resolution;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
			distance=(float) r[(i*angle/2+ernest.m_orientation-90+720)%360];
			if (distance<=10){
				m_tactilePressure[i]= 1- distance/10;
				m_tactileObject[i/angle]=c[(i*angle/2+ernest.m_orientation-90+720)%360];
			}
			else{
				m_tactilePressure[i]=0;
				m_tactileObject[i/angle]=Color.black;
			}
		}*/
		
		
		// compute neuron "capacitor"
		for (int i=0;i<resolution;i++){
			if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= 20;
			else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-20;
			else{
				if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
				else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
			}
		}
		
		
		// compute relation between neurons
		for (int i=0;i<resolution;i++){
			for (int j=0;j<resolution;j++){
				if (i!=j){
					if ( (m_tactileVariations[i]== 20 && m_tactileVariations[j]>0) 
					  || (m_tactileVariations[i]==-20 && m_tactileVariations[j]<0) ){
						
						connections[i][j]= (connections[i][j]*confidence[i][j]
						                   + Math.abs(m_tactileVariations[j]))/(confidence[i][j]+1);
						if (confidence[i][j]<10000) confidence[i][j]++;
					}
					else{
						if (Math.abs(m_tactileVariations[i])== 20){
							connections[i][j]= (connections[i][j]*confidence[i][j])/(confidence[i][j]+1);
							if (confidence[i][j]<10000) confidence[i][j]++;
						}
						
					}
				}
			}
		}
		
		
		// change distance between neuron
		double dist,dist2,dist3;
		for (int k=0;k<100;k++){
			sensorX2=sensorX.clone();
			sensorY2=sensorY.clone();
			for (int i=0;i<resolution;i++){
				for (int j=0;j<resolution;j++){
					if (i!=j && connections[i][j]>0.1){
						dist2= (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]);
						dist = Math.sqrt(dist2);
						dist3=dist/5;
						
						sensorX2[i]+= (20-Math.abs(connections[i][j])-dist3)*( (sensorX[i]-sensorX[j]) / dist )*attraction ;
						sensorY2[i]+= (20-Math.abs(connections[i][j])-dist3)*( (sensorY[i]-sensorY[j]) / dist )*attraction ;
						
						sensorX2[j]-= (20-Math.abs(connections[i][j])-dist3)*( (sensorX[i]-sensorX[j]) / dist )*attraction ;
						sensorY2[j]-= (20-Math.abs(connections[i][j])-dist3)*( (sensorY[i]-sensorY[j]) / dist )*attraction ;
					}
				}
			}
			sensorX=sensorX2;
			sensorY=sensorY2;
		}

		normalize();
		
		///////////////////////////////////////////////////////
		
		// reset potential map
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMap[i][j]=0;
			}
		}
		
		// set potentialMap values
		float scale=400/mapSize;
		int x,y;
		for (int i=0;i<resolution;i++){
			x= Math.min(49, Math.max(0, (int) ((sensorX[i]+200)/scale)));
			y= Math.min(49, Math.max(0, (int) ((sensorY[i]+200)/scale)));
			potentialMap[x][y] = Math.max(potentialMap[x][y], m_tactilePressure[i]);
			if (x-1>=0 && y-1>=0) potentialMap[x-1][y-1] = Math.max(potentialMap[x-1][y-1], m_tactilePressure[i]/2);
			if (x-1>=0          ) potentialMap[x-1][y  ] = Math.max(potentialMap[x-1][y  ], m_tactilePressure[i]/2);
			if (x-1>=0 && y+1<50) potentialMap[x-1][y+1] = Math.max(potentialMap[x-1][y+1], m_tactilePressure[i]/2);
			if (          y-1>=0) potentialMap[x  ][y-1] = Math.max(potentialMap[x  ][y-1], m_tactilePressure[i]/2);
			if (          y+1<50) potentialMap[x  ][y+1] = Math.max(potentialMap[x  ][y+1], m_tactilePressure[i]/2);
			if (x+1<50 && y-1>=0) potentialMap[x+1][y-1] = Math.max(potentialMap[x+1][y-1], m_tactilePressure[i]/2);
			if (x+1<50          ) potentialMap[x+1][y  ] = Math.max(potentialMap[x+1][y  ], m_tactilePressure[i]/2);
			if (x+1<50 && y+1<50) potentialMap[x+1][y+1] = Math.max(potentialMap[x+1][y+1], m_tactilePressure[i]/2);
		}
		
		// set chargeMap value
		for (int i=1;i<mapSize-1;i++){
			for (int j=1;j<mapSize-1;j++){
				
				// if cell need to be charged
				if (potentialMap[i][j] > chargeMap[i][j]){
					boolean empty=false;
					while (!empty && potentialMap[i][j]>chargeMap[i][j]){
						empty=true;
						empty=empty && transfert(i,j,-1,-1);
						empty=empty && transfert(i,j,-1, 0);
						empty=empty && transfert(i,j,-1, 1);
						
						empty=empty && transfert(i,j, 0,-1);
						empty=empty && transfert(i,j, 0, 1);
				
						empty=empty && transfert(i,j, 1,-1);
						empty=empty && transfert(i,j, 1, 0);
						empty=empty && transfert(i,j, 1, 1);

					}
					chargeMap[i][j]=potentialMap[i][j];
				}
				
			}
		}
		for (int i=1;i<mapSize-1;i++){
			for (int j=1;j<mapSize-1;j++){
				// if cell need to discharge
				if (potentialMap[i][j] < chargeMap[i][j]){
					chargeMap[i][j]=chargeMap[i][j]*(float)0.99;
				}
			}
		}
		
		////////////////////////////////////////////////////////////////////////
	}
	
	
	public boolean transfert(int i,int j,int x,int y){
		if (potentialMap[i][j]>potentialMap[i+x][j+y] && chargeMap[i+x][j+y]>0){
			chargeMap[i  ][j  ]+=0.05;
			chargeMap[i+x][j+y]-=0.05;
			if(chargeMap[i+x][j+y]<0) chargeMap[i+x][j+y]=0;
			
			return false;
		}
		else return true;
	}
	
	public void normalize(){
		float mx=0;
		float my=0;
		double d;
		// compute average position
		for (int i=0;i<resolution;i++){
			mx+=sensorX[i];
			my+=sensorY[i];
			
		}
		mx=mx/resolution;
		my=my/resolution;
		
		//
		double a=0;
		for (int i=0;i<resolution;i++){
			sensorX[i]-=mx;
			sensorY[i]-=my;
		}
		d=Math.sqrt( sensorX[resolution/2]*sensorX[resolution/2] + sensorY[resolution/2]*sensorY[resolution/2]);
		angle= Math.sin(sensorX[resolution/2]/d);
		
		for (int i=0;i<resolution;i++){
			sensorX[i]= sensorX[i]*Math.cos(-angle) - sensorY[i]*Math.sin(-angle);
			sensorY[i]= sensorX[i]*Math.sin(-angle) + sensorY[i]*Math.cos(-angle);
		}
		
	}
	
	
}
