
public class TactileMap {

	public float[] m_tactilePressure;
	public float[] m_tactilePressureOld;
	public int[] m_tactileVariations;
	public double[][] connections;
	public float[] m_tactileObject;
	public double[] sensorX,sensorX2;
	public double[] sensorY,sensorY2;
	public double attraction,repulsion;
	public ErnestModel ernest;
	public int resolution;
	
	public TactileMap(ErnestModel e){
		resolution=18;
		m_tactilePressure=new float[resolution];
		m_tactilePressureOld=new float[resolution];
		m_tactileVariations=new int[resolution];
		m_tactileObject=new float[resolution];
		connections=new double[resolution][resolution];
		sensorX=new double[resolution];
		sensorY=new double[resolution];
		for (int i=0;i<resolution;i++){
			m_tactilePressure[i]=0;
			m_tactilePressureOld[i]=0;
			m_tactileVariations[i]=0;
			for (int j=0;j<resolution;j++){
				connections[i][j]=100;
			}
			sensorX[i]= (float) (Math.random()*200-100);//(int)( -100*Math.sin(10*i*Math.PI/180));
			sensorY[i]= (float) (Math.random()*200-100);//(int)(  100*Math.cos(10*i*Math.PI/180));
		}
		attraction=0.0000002;
		repulsion=0.0000005;
		ernest=e;
	}
	
	public void touchEnvironment(double[] r){
		float distance;
		int angle=360/resolution;
		
		// sensors around ernest
		for (int i=0;i<360;i+=angle){
			m_tactilePressureOld[i/angle]=m_tactilePressure[i/angle];
			distance=(float) r[(i+ernest.m_orientation-180+720)%360];
			if (distance<=10) m_tactilePressure[i/angle]= 1- distance/10;
			else              m_tactilePressure[i/angle]=0;
		}
		
		// sensors in front of ernest
		/*for (int i=0;i<resolution;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
			distance=(float) r[(i*angle/2+ernest.m_orientation-90+720)%360];
			if (distance<=10) m_tactilePressure[i]= 1- distance/10;
			else              m_tactilePressure[i]=0;
		}*/
		
		
		for (int i=0;i<resolution;i++){
			if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= 5;
			else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-5;
			else{
				if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
				else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
			}
		}
		
		
		/*
		for (int i=0;i<resolution;i++){
			for (int j=0;j<resolution;j++){
				if (i!=j){
					if ( (m_tactilePressure[i]>m_tactilePressureOld[i] && m_tactileVariations[j]>0) 
					  || (m_tactilePressure[i]<m_tactilePressureOld[i] && m_tactileVariations[j]<0) ){
						if (connections[i][j]>0.1) connections[i][j]=connections[i][j]*(float)0.999;
					}
					else{
						if (m_tactilePressure[i]!=m_tactilePressureOld[i]){
							if (connections[i][j]<1000) connections[i][j]=connections[i][j]*(float)1.001;
						}
					}
				}
			}
		}
		
		
		for (int k=0;k<100;k++){
			sensorX2=sensorX.clone();
			sensorY2=sensorY.clone();
			for (int i=0;i<resolution;i++){
				for (int j=0;j<resolution;j++){
					if (i!=j){
						double d= Math.sqrt( (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]) );
						
						sensorX2[j]+= (d-connections[i][j])*(sensorX[i]-sensorX[j])*0.0000001;
						sensorY2[j]+= (d-connections[i][j])*(sensorY[i]-sensorY[j])*0.0000001;
					}
				}
			}
			sensorX=sensorX2;
			sensorY=sensorY2;
		}*/
		
		
		for (int k=0;k<100;k++){
			sensorX2=sensorX.clone();
			sensorY2=sensorY.clone();
			for (int i=0;i<resolution;i++){
				for (int j=0;j<resolution;j++){
					if (i!=j){
							if ( (m_tactilePressure[i]>m_tactilePressureOld[i] && m_tactileVariations[j]>0) 
							  || (m_tactilePressure[i]<m_tactilePressureOld[i] && m_tactileVariations[j]<0) ){
								if (sensorX[i]-sensorX[j]>0) sensorX2[j]= sensorX[j]+ (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j])*Math.abs(m_tactileVariations[j])*attraction;
								if (sensorX[i]-sensorX[j]<0) sensorX2[j]= sensorX[j]- (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j])*Math.abs(m_tactileVariations[j])*attraction;
								
								if (sensorY[i]-sensorY[j]>0) sensorY2[j]= sensorY[j]+ (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j])*Math.abs(m_tactileVariations[j])*attraction;
								if (sensorY[i]-sensorY[j]<0) sensorY2[j]= sensorY[j]- (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j])*Math.abs(m_tactileVariations[j])*attraction;
							}
							else{
								if (m_tactilePressure[i]!=m_tactilePressureOld[i]){
									if (sensorX[i]-sensorX[j]>0) sensorX2[j]= sensorX[j]- (200-(sensorX[i]-sensorX[j]))*repulsion;
									if (sensorX[i]-sensorX[j]<0) sensorX2[j]= sensorX[j]+ (200-(sensorX[j]-sensorX[i]))*repulsion;
								
									if (sensorY[i]-sensorY[j]>0) sensorY2[j]= sensorY[j]- (200-(sensorY[i]-sensorY[j]))*repulsion;
									if (sensorY[i]-sensorY[j]<0) sensorY2[j]= sensorY[j]+ (200-(sensorY[j]-sensorY[i]))*repulsion;
								}
							}
					}
				}
			}
			sensorX=sensorX2;
			sensorY=sensorY2;
		}
		
		float mx=0;
		float my=0;
		float d=0;
		for (int i=0;i<resolution;i++){
			mx+=sensorX[i];
			my+=sensorY[i];
			d+= Math.sqrt(sensorX[i]*sensorX[i] + sensorY[i]*sensorY[i]);
		}
		mx=mx/36;
		my=my/36;
		d=d/36;
		
		for (int i=0;i<resolution;i++){
			sensorX[i]-=mx;
			sensorY[i]-=my;
			
			sensorX[i]=sensorX[i]* 30/d;
			sensorY[i]=sensorY[i]* 30/d;
		}
		
		
	}
	
	
}
