
public class TactileMap {

	public float[] m_tactilePressure;
	public float[] m_tactilePressureOld;
	public float[] m_tactileVariations;
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
		m_tactileVariations=new float[resolution];
		m_tactileObject=new float[resolution];
		connections=new double[resolution][resolution];
		sensorX=new double[resolution];
		sensorY=new double[resolution];
		for (int i=0;i<resolution;i++){
			m_tactilePressure[i]=0;
			m_tactilePressureOld[i]=0;
			m_tactileVariations[i]=0;
			for (int j=0;j<resolution;j++){
				connections[i][j]=0;
			}
			sensorX[i]= (float) (Math.random()*200-100);//(int)( -100*Math.sin(10*i*Math.PI/180));
			sensorY[i]= (float) (Math.random()*200-100);//(int)(  100*Math.cos(10*i*Math.PI/180));
		}
		attraction=0.00000005;
		//repulsion=0.00000000001;
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
			if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= 10;
			else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-10;
			else{
				if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
				else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
			}
		}
		
		
		double dist,dist2;
		for (int k=0;k<100;k++){
			sensorX2=sensorX.clone();
			sensorY2=sensorY.clone();
			for (int i=0;i<resolution;i++){
				for (int j=0;j<resolution;j++){
					if (i!=j){
							if ( (m_tactileVariations[i]== 10 && m_tactileVariations[j]>0) 
							  || (m_tactileVariations[i]==-10 && m_tactileVariations[j]<0) ){
								
								dist2= (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]);
								dist = Math.sqrt(dist2);
								
								
								if (dist>=10){
									sensorX2[j]= sensorX[j]
									                     + dist2 * ( (sensorX[i]-sensorX[j]) / dist )
									                     *Math.abs(m_tactileVariations[j])
									                     *attraction
									                     ;//+(Math.random()*0.00000000002-0.00000000001);
								
									sensorY2[j]= sensorY[j]
									                     + dist2 * ( (sensorY[i]-sensorY[j]) / dist )
									                     *Math.abs(m_tactileVariations[j])
									                     *attraction
									                     ;//+(Math.random()*0.00000000002-0.00000000001);
									
								}
								else{
									sensorX2[j]-= (10-dist)* (sensorX[i]-sensorX[j]) / dist ;
									sensorY2[j]-= (10-dist)* (sensorY[i]-sensorY[j]) / dist ;
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
			
		}
		mx=mx/resolution;
		my=my/resolution;
		
		for (int i=0;i<resolution;i++){
			sensorX[i]-=mx;
			sensorY[i]-=my;
			d+= Math.sqrt(sensorX[i]*sensorX[i] + sensorY[i]*sensorY[i]);
		}
		d=d/resolution;
		for (int i=0;i<resolution;i++){
			sensorX[i]=sensorX[i]* 50/d;
			sensorY[i]=sensorY[i]* 50/d;
		}
	}
	
	
}
