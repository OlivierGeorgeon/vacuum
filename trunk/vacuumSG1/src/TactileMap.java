import java.awt.Color;
import java.util.ArrayList;


public class TactileMap {

	public float[] m_tactilePressure;
	public float[][] m_constraints;
	public float[] m_tactilePressureOld;
	public float[] m_tactileVariations;
	public double[][] connections;
	public double[][] confidence;
	public double angle;
	public Color[] m_tactileObject;
	public double[] sensorX;
	public double[] sensorY;
	//public double[] sensorR;
	//public double[] sensorT;
	public double attraction,repulsion;
	public ErnestModel ernest;
	public int resolution;
	
	public int sensorRes;
	
	public float chargeMap0[][][];
	public float chargeMap1[][][];
	public float objectMap[][][];
	public float potentialMap[][];
	public float potentialMapOld[][];
	public float potentialConfidenceMap[][];
	public boolean testMap[][];
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
	public ArrayList<float[][]> confidenceFlow;
	public ArrayList<float[][][]> flowLineX1;		// flow line chains
	public ArrayList<float[][][]> flowLineY1;
	public ArrayList<float[][][]> flowLineX2;		// extrapolated flow line chains
	public ArrayList<float[][][]> flowLineY2;
	public int mapSize;
	public int flowLength;
	
	public ArrayList<Float> mTranslationX;
	public ArrayList<Float> mTranslationY;
	public ArrayList<Float> mRotation;
	
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
		m_constraints=new float[resolution*sensorRes][resolution*sensorRes];
		connections=new double[resolution*sensorRes][resolution*sensorRes];
		confidence =new double[resolution*sensorRes][resolution*sensorRes];
		sensorX=new double[resolution*sensorRes];
		sensorY=new double[resolution*sensorRes];
		//sensorR=new double[resolution*sensorRes];
		//sensorT=new double[resolution*sensorRes];
		attraction=0.005;
		
		
		chargeMap0=new float[mapSize][mapSize][2];
		chargeMap1=new float[mapSize][mapSize][2];
		objectMap=new float[mapSize][mapSize][2];
		potentialMap=new float[mapSize][mapSize];
		potentialMapOld=new float[mapSize][mapSize];
		potentialConfidenceMap=new float[mapSize][mapSize];
		testMap=new boolean[mapSize][mapSize];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
		confidenceFlow=new ArrayList<float[][]>();
		
		flowLength=60;
		flowLineX1=new ArrayList<float[][][]>();
		flowLineY1=new ArrayList<float[][][]>();
		flowLineX2=new ArrayList<float[][][]>();
		flowLineY2=new ArrayList<float[][][]>();
		
		mTranslationX=new ArrayList<Float>();
		mTranslationY=new ArrayList<Float>();
		mRotation    =new ArrayList<Float>();
		
		for (int i=0;i<resolution*sensorRes;i++){
			m_tactilePressure[i]=0;
			m_tactilePressureOld[i]=0;
			m_tactileVariations[i]=0;
			for (int j=0;j<resolution*sensorRes;j++){
				connections[i][j]=0;
				confidence[i][j]=0;
				m_constraints[i][j]=0;
			}
			
			// initialize neurons positions
			//sensorX[i]= (float) (Math.random()*100-50);//-50*Math.sin(360/resolution*sensorRes*i*Math.PI/180);
			//sensorY[i]= (float) (Math.random()*100-50);// 50*Math.cos(360/resolution*sensorRes*i*Math.PI/180);
			/*sensorR[i]= Math.sqrt(sensorX[i]*sensorX[i] + sensorY[i]*sensorY[i]);
			if (sensorR[i]<0.01) sensorR[i]=0.01;
			
			sensorT[i]= Math.acos(sensorX[i]/sensorR[i]);
			if (sensorY[i]<0) sensorT[i]=-sensorT[i];*/
		}
		
		for (int j=0;j<sensorRes;j++){
			for (int i=0;i<resolution;i++){
				sensorX[i+j*resolution]= -(20+j*10)*Math.sin(360/resolution*i*Math.PI/180);
				sensorY[i+j*resolution]=  (20+j*10)*Math.cos(360/resolution*i*Math.PI/180);
			}
		}
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j][0]=0;
				chargeMap0[i][j][1]=0;
				chargeMap1[i][j][0]=0;
				chargeMap1[i][j][1]=0;
				potentialMap[i][j]=0;
				testMap[i][j]=false;
				
			}
		}
		
		counter=0;
	}
	

	
	
	public void touchEnvironment(double[] r,Color[] c, int act,float speed){
		
		
		for (int i=0;i<resolution*sensorRes;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
		}
		
		
		
		////////////////////////////////////////////////////////
		// set sensors values
		////////////////////////////////////////////////////////
		
		// sensors around ernest
		senseAround(r,c);
		
		// sensors in front of ernest
		//senseFront(r,c);
		
		
		///////////////////////////////////////////////////////////////
		// place neurons on the map
		///////////////////////////////////////////////////////////////
		
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
		double rad1,rad2;
		double a1,a2;
		double xp,yp;
		double dist,dist2,dist3,distP;
		for (int k=0;k<5;k++){
			for (int i=0;i<resolution*sensorRes;i++){
				for (int j=0;j<resolution*sensorRes;j++){
					if (i!=j && connections[i][j]>0.5){
						dist2= (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]);
						dist = Math.sqrt(dist2);
						dist3=dist/5;
						
						rad1= sensorX[i]*sensorX[i] + sensorY[i]*sensorY[i];
						rad1= Math.sqrt(rad1);
						rad1=Math.max(rad1, 0.001);
						
						rad2= sensorX[j]*sensorX[j] + sensorY[j]*sensorY[j];
						rad2= Math.sqrt(rad2);
						rad2=Math.max(rad2, 0.001);
						/*
						a1= Math.acos(sensorX[i]/rad1);
						if (sensorY[i]<0) a1=-a1;
						
						a2= Math.acos(sensorX[j]/rad2);
						if (sensorY[j]<0) a2=-a2;
						
						xp=a2-a1;
						if (xp>=180) xp-=360;
						if (xp<-180) xp+=360;
						
						yp=rad2-rad1;
						
						distP= xp*xp +yp*yp;
						distP= Math.sqrt(dist3);*/ 
						
						
							sensorX[i]+= ((20-connections[i][j])/*( Math.sqrt(rad1+rad2)/2 )/5*/ -dist3)
							              *( (sensorX[i]-sensorX[j]) / dist )*attraction;
							
							sensorY[i]+= ((20-connections[i][j])/*( Math.sqrt(rad1+rad2)/2 )/5*/ -dist3)
							              *( (sensorY[i]-sensorY[j]) / dist )*attraction;
							
							m_constraints[i][j]=(float) ((20-connections[i][j])-dist3);

					}
				}
			}
		}
		normalize();
		
		
		///////////////////////////////////////////////////////
		// compute flow
		///////////////////////////////////////////////////////
		/*
		// add new flow map
		if (flowX1.size()<act+1){
			while (flowX1.size()<act+1){
				flowX1.add(new float[mapSize][mapSize]);
				flowY1.add(new float[mapSize][mapSize]);
				flowX2.add(new float[mapSize][mapSize]);
				flowY2.add(new float[mapSize][mapSize]);
				confidenceFlow.add(new float[mapSize][mapSize]);
				
				flowLineX1.add(new float[mapSize][mapSize][flowLength]);
				flowLineY1.add(new float[mapSize][mapSize][flowLength]);
				flowLineX2.add(new float[mapSize][mapSize][flowLength]);
				flowLineY2.add(new float[mapSize][mapSize][flowLength]);
				
				mTranslationX.add((float) 0);
				mTranslationY.add((float) 0);
				mRotation.add((float) 0);
				
				for (int i=0;i<mapSize;i++){
					for (int j=0;j<mapSize;j++){
						flowX1.get(act)[i][j]=0;
						flowY1.get(act)[i][j]=0;
						flowX2.get(act)[i][j]=0;
						flowY2.get(act)[i][j]=0;
						confidenceFlow.get(act)[i][j]=0;
						for (int k=0;k<flowLength;k++){
							flowLineX1.get(act)[i][j][k]=0;
							flowLineY1.get(act)[i][j][k]=0;
							flowLineX2.get(act)[i][j][k]=0;
							flowLineY2.get(act)[i][j][k]=0;
						}
					}
				}
			}
		}
		
		
		// reset maps
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMapOld[i][j]=potentialMap[i][j];
				potentialMap[i][j]=0;
				potentialConfidenceMap[i][j]=0;
				testMap[i][j]=false;
				//chargeMap0[i][j]=0;
			}
		}
		
		
		// set potential and charge Map values
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
							if (d<1){
								potentialMap[x+j][y+k]=  (  potentialMap[x+j][y+k]*potentialConfidenceMap[x+j][y+k]
							                               +  m_tactilePressure[i]        )
							                              /( potentialConfidenceMap[x+j][y+k]+ 1);
								potentialConfidenceMap[x+j][y+k]++;
							}
							else{
								potentialMap[x+j][y+k]=  (  potentialMap[x+j][y+k]*potentialConfidenceMap[x+j][y+k]
							                               +  m_tactilePressure[i]*( 1/(float)d  )        )
							                              /( potentialConfidenceMap[x+j][y+k]+ 1/(float)d);
								potentialConfidenceMap[x+j][y+k]+= 1/(float)d;
							}
							
							//chargeMap0[x+j][y+k][0]=potentialMap[x+j][y+k];
							testMap[x+j][y+k]=true;
							
							if (m_tactileObject[i].equals(new Color(0,128,0)))
								chargeMap0[x+j][y+k][0]=potentialMap[x+j][y+k];
							else
								chargeMap0[x+j][y+k][1]=potentialMap[x+j][y+k];
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
				if ( Math.abs(potentialMapOld[i-l][j]-potentialMapOld[i+l][j])>0.005
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i+l][j])>0.002
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i-l][j])>0.002
				  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMapOld[i+l][j]<=potentialMapOld[i][j])
					 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMapOld[i+l][j]>=potentialMapOld[i][j]) )
					 
				  && Math.abs(potentialMapOld[i][j-l]-potentialMapOld[i][j+l])>0.005
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j+l])>0.002
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j-l])>0.002
				  && ( ( potentialMapOld[i][j-l]>=potentialMapOld[i][j] && potentialMapOld[i][j+l]<=potentialMapOld[i][j])
					 ||( potentialMapOld[i][j-l]<=potentialMapOld[i][j] && potentialMapOld[i][j+l]>=potentialMapOld[i][j]) )
				
					 
				  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
				  && potentialMap[i+l][j]>0 && potentialMap[i+l][j]<1 && potentialMapOld[i+l][j]>0 && potentialMapOld[i+l][j]<1
				  && potentialMap[i-l][j]>0 && potentialMap[i-l][j]<1 && potentialMapOld[i-l][j]>0 && potentialMapOld[i-l][j]<1
				  && potentialMap[i][j+l]>0 && potentialMap[i][j+l]<1 && potentialMapOld[i][j+l]>0 && potentialMapOld[i][j+l]<1
				  && potentialMap[i][j-l]>0 && potentialMap[i][j-l]<1 && potentialMapOld[i][j-l]>0 && potentialMapOld[i][j-l]<1
				  
				  && testMap[i][j] && testMap[i-l][j] && testMap[i+l][j] && testMap[i][j-l] && testMap[i][j+l]
				  && speed>1){

					
					//fx=((potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]));
					//fy=((potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]));
					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]) );
					
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]) );
					
					//System.out.println("=================== "+speed+" ; "+fx+ " ; "+fy);
					
					
					if (fx!=0 || fy!=0){
					
						flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx ) 
						                      /(confidenceFlow.get(act)[i][j]+1);
						flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy ) 
						                      /(confidenceFlow.get(act)[i][j]+1);
					
						if (confidenceFlow.get(act)[i][j]<10000) confidenceFlow.get(act)[i][j]++;
					}
					
				}
				else if (!testMap[i][j]){
					flowX1.get(act)[i][j]=0;
					flowY1.get(act)[i][j]=0;
					confidenceFlow.get(act)[i][j]=0;
				}
			}
		}
		
		// reduce noise
		int count;
		float mx,my;
		int a=5;
		int k=act;
		int i3,j3;
		//for (int k=0;k<flowX1.size();k++){
			for (int i=a;i<mapSize-a;i++){
				for (int j=a;j<mapSize-a;j++){
					flowX2.get(k)[i][j]=0;
					flowY2.get(k)[i][j]=0;
					count=0;
					mx=0;
					my=0;
					for (int i2=-a;i2<=a;i2++){
						for (int j2=-a;j2<=a;j2++){
							i3=i+i2;
							j3=j+j2;
							if (flowX1.get(k)[i3][j3]!=0 || flowY1.get(k)[i3][j3]!=0){
								count++;
								mx+=flowX1.get(k)[i3][j3];
								my+=flowY1.get(k)[i3][j3];
							}
						}
					}
					
					if (count>30 && (mx!=0 || my!=0)){
						mx=mx/(float)count;
						my=my/(float)count;
						
						
						flowX2.get(k)[i][j]=mx;
						flowY2.get(k)[i][j]=my;
						
					}
					
				}
			}
		//}
		*/
			
		
		////////////////////////////////////////////////////////////////////////
		// compute average translation and rotation vectors
		////////////////////////////////////////////////////////////////////////
		/*
		count=0;
		int count2=0;
		mx=0;
		my=0;
		float mTheta=0;
		double theta0,theta1;
		for (int i=a;i<mapSize-a;i++){
			for (int j=a;j<mapSize-a;j++){
				if (testMap[i][j]){
					// translation
					mx+=flowX2.get(k)[i][j];
					my+=flowY2.get(k)[i][j];
					
					//rotation
					
					// theta0
					if ((j-mapSize/2)!=0){
						theta0= 2*Math.atan( (j-mapSize/2) 
								            / ( (i-mapSize/2) + Math.sqrt((i-mapSize/2)*(i-mapSize/2)+(j-mapSize/2)*(j-mapSize/2)) ) 
								           );
					}
					else{
						if ((i-mapSize/2)>=0) theta0=0;
						else                  theta0=Math.PI;
					}
					
					// theta1
					if ((j-mapSize/2)!=0){
						theta1= 2*Math.atan( (j-mapSize/2+flowY2.get(k)[i][j]) 
								            / ( (i-mapSize/2+flowX2.get(k)[i][j]) 
								               + Math.sqrt( (i-mapSize/2+flowX2.get(k)[i][j])*(i-mapSize/2+flowX2.get(k)[i][j])
								                           +(j-mapSize/2+flowY2.get(k)[i][j])*(j-mapSize/2+flowY2.get(k)[i][j]))
								              )
								           );
					}
					else{
						if ((i-mapSize/2)>=0) theta1=0;
						else                  theta1=Math.PI;
					}

					
					mTheta+= theta1-theta0;
					
					count++;
				}
			}
		}
		mTranslationX.set(act, mx/(float)count);
		mTranslationY.set(act, my/(float)count);
		mRotation.set(act, mTheta/(float)count);
		
		// set the extrapolated flow field
		fx=0;
		fy=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (!testMap[i][j]){
					
					fx= (float) ((float)(i-mapSize/2)*Math.cos(mRotation.get(act)) - (float)(j-mapSize/2)*Math.sin(mRotation.get(act)));
					fy= (float) ((float)(i-mapSize/2)*Math.sin(mRotation.get(act)) + (float)(j-mapSize/2)*Math.cos(mRotation.get(act)));
					
					fx-=(float)(i-mapSize/2);
					fy-=(float)(j-mapSize/2);
					
					flowX2.get(act)[i][j]=fx +mTranslationX.get(act);
					flowY2.get(act)[i][j]=fy +mTranslationY.get(act);
					
					
					// corrections near known flow
					count=0;
					count2=0;
					mx=0;
					my=0;
					for (int i2=-6;i2<6;i2++){
						for (int j2=-6;j2<6;j2++){
							if ( (i+i2)>=0 && (i+i2)<mapSize && (j+j2)>=0 && (j+j2)<mapSize){
								if (testMap[i+i2][j+j2]){
									mx+=flowX2.get(k)[i+i2][j+j2];
									my+=flowY2.get(k)[i+i2][j+j2];
									count++;
								}
								count2++;
							}
						}
					}
					
					if (count>1 && count2>1){
						count2=Math.max(count2/2,count);
						flowX2.get(act)[i][j]= (flowX2.get(act)[i][j]*(count2-count) + mx )/count2;
						flowY2.get(act)[i][j]= (flowY2.get(act)[i][j]*(count2-count) + my )/count2;
					}
					
				}
			}
		}
		*/
		
		////////////////////////////////////////////////////////////////////////
		// move charges
		////////////////////////////////////////////////////////////////////////
		/*
		mx=my=0;
		d=0;
		float countD=0;
		float chargeSum0=0;
		float chargeSum1=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (!testMap[i][j]){
					mx=(float)i-flowX2.get(act)[i][j]*speed;
					my=(float)j-flowY2.get(act)[i][j]*speed;
			
					int ix=Math.round(mx);
					int jy=Math.round(my);
				
					chargeSum0=0;
					chargeSum1=0;
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (ix+i2>=0 && ix+i2<mapSize && jy+j2>=0 && jy+j2<mapSize){
								d= ((float)(ix+i2)-mx)*((float)(ix+i2)-mx) 
							      +((float)(jy+j2)-my)*((float)(jy+j2)-my);
								d=Math.min(1,Math.sqrt(d));
								chargeSum0+=chargeMap0[ix+i2][jy+j2][0]*(1-d);
								chargeSum1+=chargeMap0[ix+i2][jy+j2][1]*(1-d);
								countD+=(1-d);
							}
						}
					}
				
					if (countD>0){
						chargeSum0=chargeSum0/countD;
						chargeSum1=chargeSum1/countD;
						chargeMap1[i][j][0]=(float) Math.min(1,chargeSum0);
						chargeMap1[i][j][1]=(float) Math.min(1,chargeSum1);
					}
				}

			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j][0]=Math.min(1,chargeMap1[i][j][0]);
				chargeMap0[i][j][1]=Math.min(1,chargeMap1[i][j][1]);
				chargeMap1[i][j][0]=0;
				chargeMap1[i][j][1]=0;		
			}
		}
		*/
		
		
		////////////////////////////////////////////////////////////////////////
		// estimate flow lines
		////////////////////////////////////////////////////////////////////////
		/*
		float xl,yl;
		int Ixl,Iyl;
		//for (int k=0;k<flowLineX1.size();k++){
			for (int i=a;i<mapSize-a;i+=2){
				for (int j=a;j<mapSize-a;j+=2){
					
					flowLineX1.get(k)[i][j][0]=0;
					flowLineY1.get(k)[i][j][0]=0;
					
					
					if (flowX2.get(k)[i][j]!=0 || flowY2.get(k)[i][j]!=0){
						
						flowLineX1.get(k)[i][j][0]=i;
						flowLineY1.get(k)[i][j][0]=j;
						
						xl=i;
						yl=j;
						Ixl=i;
						Iyl=j;
						
						int n=1;
						boolean test=true;
						while (n<flowLength && test){
							
							count=0;
							int count2=0;
							mx=0;
							my=0;
							d=0;
							
							Ixl=Math.round(xl);
							Iyl=Math.round(yl);
							
							int a2=1;
							for (int i2=-a2;i2<a2;i2++){
								for (int j2=-a2;j2<a2;j2++){
										
									d= (xl-(float)(Ixl+i2))*(xl-(float)(Ixl+i2)) + (yl-(float)(Iyl+j2))*(yl-(float)(Iyl+j2));
									d=Math.sqrt(d);
										
									if (d<0.1) d=0.1;
										
									count+=1/d;
									count2++;
									mx+=flowX2.get(k)[Ixl+i2][Iyl+j2]/d;
									my+=flowY2.get(k)[Ixl+j2][Iyl+j2]/d;
								}
							}
							
							if (count2>3 && (mx!=0 || my!=0)){
								
								mx=mx/count;
								my=my/count;
								
								flowLineX1.get(k)[i][j][n]=flowLineX1.get(k)[i][j][n-1]+mx/20;
								flowLineY1.get(k)[i][j][n]=flowLineY1.get(k)[i][j][n-1]+my/20;
								
								xl=flowLineX1.get(k)[i][j][n];
								yl=flowLineY1.get(k)[i][j][n];
							}
							else{
								test=false;
								flowLineX1.get(k)[i][j][n]=0;
								flowLineY1.get(k)[i][j][n]=0;
							}
							n++;
						}
					}
				}
			}
			
		//}
		
		//////////////////////////////////////////////////////////
		// extrapolation of flow lines
		//////////////////////////////////////////////////////////
		a=5;
		//for (int k=0;k<flowLineX1.size();k++){
			for (int i=a;i<mapSize-a;i+=2){
				for (int j=a;j<mapSize-a;j+=2){
					count=0;
					boolean n1,n2,n3,n4;
					n1=n2=n3=n4=false;
					
					flowLineX2.get(k)[i][j][0]=0;
					flowLineY2.get(k)[i][j][0]=0;
					
					if (flowLineX1.get(k)[i][j][0]==0 && flowLineY1.get(k)[i][j][0]==0){
						if (flowLineX1.get(k)[i+2][j  ][0]!=0 && flowLineY1.get(k)[i+2][j  ][0]!=0){
							n1=true;
							count++;
						}
						if (flowLineX1.get(k)[i-2][j  ][0]!=0 && flowLineY1.get(k)[i-2][j  ][0]!=0){
							n2=true;
							count++;
						}
						if (flowLineX1.get(k)[i  ][j+2][0]!=0 && flowLineY1.get(k)[i  ][j+2][0]!=0){
							n3=true;
							count++;
						}
						if (flowLineX1.get(k)[i  ][j-2][0]!=0 && flowLineY1.get(k)[i  ][j-2][0]!=0){
							n4=true;
							count++;
						}
					}
					
					// case simple extrapolation
					/*if (count>1){
						
						if (count==4){
							flowLineX2.get(k)[i][j][0]=i;
							flowLineY2.get(k)[i][j][0]=j;
							
							int n=0;
							boolean test=true;
							while (n<flowLength && test){
								if (flowLineX1.get(k)[i+4][j  ][n]!=0 && flowLineY1.get(k)[i+4][j  ][n]!=0
								  &&flowLineX1.get(k)[i-4][j  ][n]!=0 && flowLineY1.get(k)[i-4][j  ][n]!=0
								  &&flowLineX1.get(k)[i  ][j+4][n]!=0 && flowLineY1.get(k)[i  ][j+4][n]!=0
								  &&flowLineX1.get(k)[i  ][j-4][n]!=0 && flowLineY1.get(k)[i  ][j-4][n]!=0){
									
									flowLineX2.get(k)[i][j][n]=( flowLineX1.get(k)[i+4][j  ][n]
									                            +flowLineX1.get(k)[i-4][j  ][n]
									                            +flowLineX1.get(k)[i  ][j+4][n]
									                            +flowLineX1.get(k)[i  ][j-4][n]) / 4;
									
									flowLineY2.get(k)[i][j][n]=( flowLineY1.get(k)[i+4][j  ][n]
																+flowLineY1.get(k)[i-4][j  ][n]
																+flowLineY1.get(k)[i  ][j+4][n]
																+flowLineY1.get(k)[i  ][j-4][n]) / 4;
									
								}
								else{
									flowLineX2.get(k)[i][j][n]=0;
									flowLineY2.get(k)[i][j][n]=0;
									test=false;
								}
								
								n++;
							}
							
						}
						else if (n1 && n2){
							flowLineX2.get(k)[i][j][0]=i;
							flowLineY2.get(k)[i][j][0]=j;
							
							int n=0;
							boolean test=true;
							while (n<flowLength && test){
								if (flowLineX1.get(k)[i+4][j][n]!=0 && flowLineY1.get(k)[i+4][j][n]!=0
								  &&flowLineX1.get(k)[i-4][j][n]!=0 && flowLineY1.get(k)[i-4][j][n]!=0 ){
									
									flowLineX2.get(k)[i][j][n]=( flowLineX1.get(k)[i+4][j][n]
									                            +flowLineX1.get(k)[i-4][j][n]) / 2;
									
									flowLineY2.get(k)[i][j][n]=( flowLineY1.get(k)[i+4][j][n]
																+flowLineY1.get(k)[i-4][j][n]) / 2;
									
								}
								else{
									flowLineX2.get(k)[i][j][n]=0;
									flowLineY2.get(k)[i][j][n]=0;
									test=false;
								}
								
								n++;
							}
						}
						else if (n3 && n4){
							flowLineX2.get(k)[i][j][0]=i;
							flowLineY2.get(k)[i][j][0]=j;
							
							int n=0;
							boolean test=true;
							while (n<flowLength && test){
								if (flowLineX1.get(k)[i][j+4][n]!=0 && flowLineY1.get(k)[i][j+4][n]!=0
								  &&flowLineX1.get(k)[i][j-4][n]!=0 && flowLineY1.get(k)[i][j-4][n]!=0){
									
									flowLineX2.get(k)[i][j][n]=( flowLineX1.get(k)[i][j+4][n]
									                            +flowLineX1.get(k)[i][j-4][n]) / 2;
									
									flowLineY2.get(k)[i][j][n]=( flowLineY1.get(k)[i][j+4][n]
																+flowLineY1.get(k)[i][j-4][n]) / 2;
									
								}
								else{
									flowLineX2.get(k)[i][j][n]=0;
									flowLineY2.get(k)[i][j][n]=0;
									test=false;
								}
								
								n++;
							}
						}

					}*/
					/*
					// case complex extrapolation with 1 line
					if (count==1){
						
						int offsetX=0;
						int offsetY=0;
						if (n1) offsetX=2;
						else if (n2) offsetX=-2;
						else if (n3) offsetY=2;
						else if (n4) offsetY=-2;
						
						if ( flowLineX1.get(k)[i+offsetX*2][j+offsetY*2][0]!=0  
						 &&  flowLineY1.get(k)[i+offsetX*2][j+offsetY*2][0]!=0 ){
							
							int n=1;
							boolean test=true;
							float d1,dx1,dy1,dx2,dy2;
							float d0=2;
							
							// initialize line
							flowLineX2.get(k)[i][j][0]=i;
							flowLineY2.get(k)[i][j][0]=j;
							
							while (n<flowLength-1 && test){
								
								dx1=flowLineX1.get(k)[i+offsetX  ][j+offsetY  ][n];
								dy1=flowLineY1.get(k)[i+offsetX  ][j+offsetY  ][n];
								
								dx2=flowLineX1.get(k)[i+offsetX*2][j+offsetY*2][n];
								dy2=flowLineY1.get(k)[i+offsetX*2][j+offsetY*2][n];
								
								d1= (dx1-dx2)*(dx1-dx2) + (dy1-dy2)*(dy1-dy2);
								d1=(float) Math.sqrt(d1);
								
								
								flowLineX2.get(k)[i][j][n]= dx1 + (dx1-dx2);
								flowLineY2.get(k)[i][j][n]= dy1 + (dy1-dy2);
								
								n++;
								
								test=  flowLineX1.get(k)[i+offsetX  ][j+offsetY  ][n]!=0  
								   &&  flowLineY1.get(k)[i+offsetX  ][j+offsetY  ][n]!=0 
								   &&  flowLineX1.get(k)[i+offsetX*2][j+offsetY*2][n]!=0  
								   &&  flowLineY1.get(k)[i+offsetX*2][j+offsetY*2][n]!=0 ;
							}
							
							if (n<flowLength-1){
								flowLineX2.get(k)[i][j][n-1]=0;
								flowLineY2.get(k)[i][j][n-1]=0;
							}
							
							
						}
						
						
					}
					
				}
			}
		//}
		*/
		
	}


	public void senseAround(double[] r,Color[] c){
		float distance,distance2;
		int angle=360/resolution;
		
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
	}
	
	
	public void senseFront(double[] r,Color[] c){
		float distance,distance2;
		int angle=180/resolution;
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
		}
	}
	
	// normalize position and angle of the neuron network
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
