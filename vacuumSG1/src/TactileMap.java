import java.awt.Color;
import java.util.ArrayList;


public class TactileMap {

	public float[] m_tactilePressure;
	public float[] m_tactilePressureOld;
	public float[] m_tactileVariations;
	public double[][] connections;
	public double[][] confidence;
	public double angle;
	public Color[] m_tactileObject;
	public double[] sensorX;
	public double[] sensorY;
	public double attraction,repulsion;
	public ErnestModel ernest;
	public int resolution;
	
	public int sensorRes;
	
	public float chargeMap[][];
	public float potentialMap[][];
	public float potentialMapOld[][];
	public float potentialConfidenceMap[][];
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
	public float confidenceFlow[][];
	public int mapSize;
	
	public int counter;
	
	
	public TactileMap(ErnestModel e){
		resolution=18;
		sensorRes=3;
		ernest=e;
		
		initialize();
	}
	
	
	public TactileMap(ErnestModel e,int res, int sensor_res){
		resolution=res;
		sensorRes=sensor_res;
		ernest=e;
		
		initialize();
	}
	
	private void initialize(){
		mapSize=50;
		m_tactilePressure=new float[resolution*sensorRes];
		m_tactilePressureOld=new float[resolution*sensorRes];
		m_tactileVariations=new float[resolution*sensorRes];
		m_tactileObject=new Color[resolution*sensorRes];
		connections=new double[resolution*sensorRes][resolution*sensorRes];
		confidence =new double[resolution*sensorRes][resolution*sensorRes];
		sensorX=new double[resolution*sensorRes];
		sensorY=new double[resolution*sensorRes];
		attraction=0.005;
		
		
		chargeMap=new float[mapSize][mapSize];
		potentialMap=new float[mapSize][mapSize];
		potentialMapOld=new float[mapSize][mapSize];
		potentialConfidenceMap=new float[mapSize][mapSize];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
		confidenceFlow=new float[mapSize][mapSize];
		
		for (int i=0;i<resolution*sensorRes;i++){
			m_tactilePressure[i]=0;
			m_tactilePressureOld[i]=0;
			m_tactileVariations[i]=0;
			for (int j=0;j<resolution*sensorRes;j++){
				connections[i][j]=0;
				confidence[i][j]=0;
			}
			
			// initialize neurons positions
			sensorX[i]= (float) (Math.random()*100-50);//-50*Math.sin(360/resolution*sensorRes*i*Math.PI/180);
			sensorY[i]= (float) (Math.random()*100-50);// 50*Math.cos(360/resolution*sensorRes*i*Math.PI/180);
		}
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap[i][j]=0;
				potentialMap[i][j]=0;
				confidenceFlow[i][j]=0;
			}
		}
		
		counter=0;
	}
	

	
	
	public void touchEnvironment(double[] r,Color[] c, int act){
		float distance,distance2;
		int angle=360/resolution;
		
		for (int i=0;i<resolution*sensorRes;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
		}
		
		// sensors around ernest
		int E_angle=ernest.m_orientation+540;
		int index=0;
		for (int i=0;i<360;i+=angle){
			distance=(float) r[(i+E_angle)%360]-5;
			for (int j=0;j<sensorRes;j++){
				if (j==0){
					index=i/angle;
					if (distance<=5){
						distance2= Math.min(1,1- distance/5);
						m_tactilePressure[index]= distance2;
						m_tactileObject[index]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[index]=0;
						m_tactileObject[index]=Color.black;
					}
				}
				else if (j==1){
					index=i/angle+resolution;
					if (distance<=7){
						distance2=(float) Math.min(1,1- (distance-2)/5);
						m_tactilePressure[index]= distance2;
						m_tactileObject[index]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[index]=0;
						m_tactileObject[index]=Color.black;
					}
				}
				else{
					index=i/angle+2*resolution;
					if (distance<=10){
						distance2=(float) Math.min(1,1- (distance-5)/5);
						m_tactilePressure[index]= distance2;
						m_tactileObject[index]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[index]=0;
						m_tactileObject[index]=Color.black;
					}
				}
			}
		}
		
		/*
		// sensors in front of ernest
		angle=180/resolution;
		int E_angle=ernest.m_orientation+630;  // 630 = -90 +720
		for (int i=0;i<180;i+=angle){
			distance=(float) r[(i+E_angle)%360]-5;
			for (int j=0;j<sensorRes;j++){
				if (j==0){
					if (distance<=5){
						distance2= Math.min(1,1- distance/5);
						m_tactilePressure[i/angle]= distance2;
						m_tactileObject[i/angle]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[i/angle]=0;
						m_tactileObject[i/angle]=Color.black;
					}
				}
				else if (j==1){
					if (distance<=7){
						distance2=(float) Math.min(1,1- (distance-2)/5);
						m_tactilePressure[i/angle+resolution]= distance2;
						m_tactileObject[i/angle+resolution]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[i/angle+resolution]=0;
						m_tactileObject[i/angle+resolution]=Color.black;
					}
				}
				else{
					if (distance<=10){
						distance2=(float) Math.min(1,1- (distance-5)/5);
						m_tactilePressure[i/angle+2*resolution]= distance2;
						m_tactileObject[i/angle+2*resolution]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[i/angle+2*resolution]=0;
						m_tactileObject[i/angle+2*resolution]=Color.black;
					}
				}

			}
		}*/
		
		
		// compute neuron "capacitor"
		for (int i=0;i<resolution*sensorRes;i++){
			if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= 20;
			else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-20;
			else{
				if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
				else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
				
				//m_tactileVariations[i]=m_tactileVariations[i]*(float)0.9;
			}
		}
		
		
		// compute relation between neurons
		for (int i=0;i<resolution*sensorRes;i++){
			for (int j=0;j<resolution*sensorRes;j++){
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
		for (int k=0;k<5;k++){
			for (int i=0;i<resolution*sensorRes;i++){
				for (int j=0;j<resolution*sensorRes;j++){
					if (i!=j && connections[i][j]>0.1){
						dist2= (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]);
						dist = Math.sqrt(dist2);
						dist3=dist/5;
						
							sensorX[i]+= (20-connections[i][j] -dist3)
							              *( (sensorX[i]-sensorX[j]) / dist )*attraction;
							
							sensorY[i]+= (20-connections[i][j] -dist3)
							              *( (sensorY[i]-sensorY[j]) / dist )*attraction;

					}
				}
			}
		}

		normalize();
		
		///////////////////////////////////////////////////////
		
		
		if (flowX1.size()<act+1){
			while (flowX1.size()<act+1){
				flowX1.add(new float[mapSize][mapSize]);
				flowY1.add(new float[mapSize][mapSize]);
				flowX2.add(new float[mapSize][mapSize]);
				flowY2.add(new float[mapSize][mapSize]);
				
				for (int i=0;i<mapSize;i++){
					for (int j=0;j<mapSize;j++){
						flowX1.get(act)[i][j]=0;
						flowY1.get(act)[i][j]=0;
						flowX2.get(act)[i][j]=0;
						flowY2.get(act)[i][j]=0;
					}
				}
			}
		}
		
		
		// reset potential map
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMapOld[i][j]=potentialMap[i][j];
				potentialMap[i][j]=0;
				potentialConfidenceMap[i][j]=0;
			}
		}
		
		
		
		
		// set potentialMap values
		float scale=400/mapSize;
		int x,y;
		double d;
		for (int i=0;i<resolution*sensorRes;i++){
			x= Math.min(49, Math.max(0, (int) ((sensorX[i]+200)/scale)));
			y= Math.min(49, Math.max(0, (int) ((sensorY[i]+200)/scale)));
			
			int dmax=3;
			for (int j=-dmax;j<=dmax;j++){
				for (int k=-dmax;k<=dmax;k++){
					if (x+j<50 && x+j>=0 && y+k<50 && y+k>=0){
						d= Math.sqrt( (float)(j*j+k*k) );
						if (d<=dmax){
							if (d==0){
								potentialMap[x+j][y+k]=  (  potentialMap[x+j][y+k]*potentialConfidenceMap[x+j][y+k]
							                               +  m_tactilePressure[i]        )
							                              /( potentialConfidenceMap[x+j][y+k]+ 1);
								potentialConfidenceMap[x+j][y+k]+=1;
							}
							else{
								potentialMap[x+j][y+k]=  (  potentialMap[x+j][y+k]*potentialConfidenceMap[x+j][y+k]
							                               +  m_tactilePressure[i]*( 1/(float)d  )        )
							                              /( potentialConfidenceMap[x+j][y+k]+ 1/(float)d);
								potentialConfidenceMap[x+j][y+k]+= 1/(float)d;
							}
						}
					}
				}
			}
		}
		
		
		// compute flow
		float fx,fy;
		int l=1;
		for (int i=l;i<mapSize-l;i++){
			for (int j=l;j<mapSize-l;j++){
				if ( Math.abs(potentialMap[i-l][j]-potentialMap[i+l][j])>0.2
				  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMap[i+l][j]<=potentialMap[i][j])
					 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMap[i+l][j]>=potentialMap[i][j]) )
					 
				  && Math.abs(potentialMap[i][j-l]-potentialMap[i][j+l])>0.2
				  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMap[i+l][j]<=potentialMap[i][j])
					 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMap[i+l][j]>=potentialMap[i][j]) )
				
				  ){
					
					fx=10*(potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMap[i-l][j]-potentialMap[i+l][j]);
					fy=10*(potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMap[i][j-l]-potentialMap[i][j+l]);
					
					if (fx!=0 || fy!=0){
					
						flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow[i][j] + fx ) / (confidenceFlow[i][j]+1);
						flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow[i][j] + fy ) / (confidenceFlow[i][j]+1);
					
						confidenceFlow[i][j]++;
					}
					
					
				}
			}
		}
		
		// reduce noise
		int count;
		float mx,my;
		int a=5;
		for (int k=0;k<flowX1.size();k++){
			for (int i=a;i<mapSize-a;i++){
				for (int j=a;j<mapSize-a;j++){
					
					count=0;
					mx=0;
					my=0;
					for (int i2=-a;i2<a;i2++){
						for (int j2=-a;j2<a;j2++){
							if (flowX1.get(k)[i+i2][j+j2]!=0 || flowY1.get(k)[i+i2][j+j2]!=0){
								count++;
								mx+=flowX1.get(k)[i+i2][j+j2];
								my+=flowY1.get(k)[i+i2][j+j2];
							}
						}
					}
					
					if (count>0 && (mx!=0 || my!=0)){
						mx=mx/(float)count;
						my=my/(float)count;
						
						
						flowX2.get(k)[i][j]=mx;
						flowY2.get(k)[i][j]=my;
						
					}
					
				}
			}
		}
		
		/*
		
		float diff=0;
		float diff2=0;
		int count;
		for (int k=0;k<50;k++){
		for (int i=1;i<mapSize-1;i++){
			for (int j=1;j<mapSize-1;j++){
				// if cell has a defined potential
				if (potentialConfidenceMap[i][j]>0){
					diff=potentialMap[i][j]-chargeMap[i][j];
					count=0;
					
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (potentialConfidenceMap[i][j]>0){
								count++;
							}
						}
					}
					
					// transfer charge
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (potentialConfidenceMap[i+i2][j+j2]>0){
								diff2=potentialMap[i+i2][j+j2]-chargeMap[i+i2][j+j2];
								transfer(i,j,i2,j2, ((float)1/(float)(count+1))*(diff-diff2));
							
								flowX[i][j]-= 50*((float)1/(float)(count+1))*(diff-diff2)*(float)i2;
								flowY[i][j]-= 50*((float)1/(float)(count+1))*(diff-diff2)*(float)j2;
							}
						}
					}
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (potentialConfidenceMap[i+i2][j+j2]==0 && chargeMap[i][j]>potentialMap[i][j]){
								transfer(i,j,i2,j2,potentialMap[i][j]-chargeMap[i][j]);
							
								flowX[i][j]-= 50*((float)1/(float)(count+1))*(diff-diff2)*(float)i2;
								flowY[i][j]-= 50*((float)1/(float)(count+1))*(diff-diff2)*(float)j2;
							}
						}
					}

				}
			}
		}
		}
		
		if (counter==0){
		// charge is created or eliminated
		for (int i=1;i<mapSize-1;i++){
			for (int j=1;j<mapSize-1;j++){
				chargeMap[i][j]=potentialMap[i][j];
			}
		}
		counter=0;
		}
		else counter--;
		*/

		
		////////////////////////////////////////////////////////////////////////
	}
	
	public void transfer(int i,int j,int x,int y,float value){
		chargeMap[i  ][j  ]+=value;
		chargeMap[i+x][j+y]-=value;
		if(chargeMap[i+x][j+y]<0) chargeMap[i+x][j+y]=0;
	}
	
	public void normalize(){
		float mx=0;
		float my=0;
		double d;
		// compute average position
		for (int i=0;i<resolution*sensorRes;i++){
			mx+=sensorX[i];
			my+=sensorY[i];
			
		}
		mx=mx/(resolution*sensorRes);
		my=my/(resolution*sensorRes);
		
		//
		for (int i=0;i<resolution*sensorRes;i++){
			sensorX[i]-=mx;
			sensorY[i]-=my;
		}
		d=Math.sqrt( sensorX[resolution/2]*sensorX[resolution/2] + sensorY[resolution/2]*sensorY[resolution/2]);
		angle= Math.sin(sensorX[resolution/2]/d);
		
		for (int i=0;i<resolution*sensorRes;i++){
			sensorX[i]= sensorX[i]*Math.cos(-angle) - sensorY[i]*Math.sin(-angle);
			sensorY[i]= sensorX[i]*Math.sin(-angle) + sensorY[i]*Math.cos(-angle);
		}

		
		
	}
	
	
}
