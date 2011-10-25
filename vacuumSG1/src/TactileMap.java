import java.awt.Color;
import java.util.ArrayList;


public class TactileMap {

	public float[] m_tactilePressure;               // value of each sensor neurons
	public float[] m_tactilePressureOld;
	
	public float[] m_tactileValue;                  // value of sensors
	
	public float[][] m_constraints;
	public double[][] m_distances;
	public float[][] m_connectionsLenght;
	
	public float[] m_tactileVariations;				// neurons "capacity"
	
	public double[][] connections;					// average distance between neurons
	public double[][] connectConfidence;
	
	public ArrayList<float[]> flowVectorX;			// movement vector on each neuron
	public ArrayList<float[]> flowVectorY;
	public ArrayList<float[]> vectorConfidence;

	public Color[] m_tactileObject;
	public double[] sensorX;                        // position of sensor neurons
	public double[] sensorY;
	public double[] valueX;							// position of the detected point
	public double[] valueY;
	public double[] valueOldX;
	public double[] valueOldY;
	public double attraction,repulsion;
	
	public ErnestModel ernest;
	
	public int resolution;                              // nb of tactile sensors
	public int sensorRes;                               // nb of neurons per sensor
	
	public float chargeMap0[][][];						// charge maps k=0 => nothing
	public float chargeMap1[][][];						//               1 => solid object
	public float chargeMapP[][][];						//               2 => soft object
														// map0 -> construction, map1-> final, mapP->polar
	public int maximumMap[][];
	public int output[][];
	
	public float objectMap[][][];

	public double polar2cartesianX[][];
	public double polar2cartesianY[][];
	
	public boolean potentialTestMap[][];
	public boolean chargeTestMap[][];
	
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
	public ArrayList<float[][]> confidenceFlow;
	
	public ArrayList<float[][][]> flowLineX1;		// flow line chains
	public ArrayList<float[][][]> flowLineY1;
	public ArrayList<float[][][]> flowLineX2;		// extrapolated flow line chains
	public ArrayList<float[][][]> flowLineY2;
	
	public int mapSize,mapSizeTheta,mapSizeR;
	public int flowLength;
	
	public ArrayList<Float> mTranslationX;
	public ArrayList<Float> mTranslationY;
	public ArrayList<Float> mRotation;
	
	public int counter;
	
	public double r,tx,ty;							// rotation and translation vector
	
	
	public TactileMap(ErnestModel e){
		resolution=18;
		sensorRes=3;
		ernest=e;
		tx=0;
		ty=0;
		r=0;
		initialize();
	}
	
	
	public TactileMap(ErnestModel e,int res, int sensor_res){
		resolution=res;
		sensorRes=sensor_res;
		ernest=e;
		
		initialize();
	}
	

	//*************************************************************************
	// initialisation
	//*************************************************************************
	private void initialize(){
		mapSize=100;
		mapSizeTheta=180;
		mapSizeR=100;
		
		m_tactilePressure=new float[resolution*sensorRes];
		m_tactileValue=new float[resolution];
		m_tactilePressureOld=new float[resolution*sensorRes];
		m_tactileVariations=new float[resolution*sensorRes];
		flowVectorX=new ArrayList<float[]>();
		flowVectorY=new ArrayList<float[]>();
		vectorConfidence=new ArrayList<float[]>();
		m_tactileObject=new Color[resolution];
		m_constraints=new float[resolution*sensorRes][resolution*sensorRes];
		m_distances=new double[resolution*sensorRes][resolution*sensorRes];
		m_connectionsLenght=new float[resolution*sensorRes][resolution*sensorRes];
		connections=new double[resolution*sensorRes][resolution*sensorRes];
		connectConfidence =new double[resolution*sensorRes][resolution*sensorRes];
		sensorX=new double[resolution*sensorRes];
		sensorY=new double[resolution*sensorRes];
		valueX=new double[resolution];
		valueY=new double[resolution];
		valueOldX=new double[resolution];
		valueOldY=new double[resolution];
		attraction=0.001;
		
		
		chargeMap0=new float[mapSize][mapSize][3];
		chargeMap1=new float[mapSize][mapSize][3];
		chargeMapP=new float[mapSizeTheta][mapSizeR][3];
		maximumMap=new int[mapSizeTheta][mapSizeR];
		output=new int[mapSizeTheta][4];
		
		objectMap=new float[mapSize][mapSize][3];
		potentialTestMap=new boolean[mapSize][mapSize];
		chargeTestMap=new boolean[mapSize][mapSize];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
		confidenceFlow=new ArrayList<float[][]>();
		
		polar2cartesianX=new double[mapSizeTheta][mapSizeR];
		polar2cartesianY=new double[mapSizeTheta][mapSizeR];
		
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
				connectConfidence[i][j]=0;
				m_constraints[i][j]=0;
				m_distances[i][j]=0.5;				
				m_connectionsLenght[i][j]=0;
			}
			
			// initialize neurons positions
			//sensorX[i]= (float) (Math.random()*100-50);//-50*Math.sin(360/resolution*sensorRes*i*Math.PI/180);
			//sensorY[i]= (float) (Math.random()*100-50);// 50*Math.cos(360/resolution*sensorRes*i*Math.PI/180);
		}
		
		
		for (int j=0;j<sensorRes;j++){
			for (int i=0;i<resolution;i++){
				float r=0;
				if (j==0) r=5;
				if (j==1) r=10;
				if (j==2) r=15;
				sensorX[i+j*resolution]= -(r)*Math.sin(360/resolution*i*Math.PI/180);
				sensorY[i+j*resolution]=  (r)*Math.cos(360/resolution*i*Math.PI/180);
			}
		}
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j][0]=0;
				chargeMap0[i][j][1]=0;
				chargeMap0[i][j][2]=0;
				
				chargeMap1[i][j][0]=0;
				chargeMap1[i][j][1]=0;
				chargeMap1[i][j][2]=0;
				potentialTestMap[i][j]=false;
				chargeTestMap[i][j]=false;
				
			}
		}
		
		for (int i=0;i<mapSizeTheta;i++){
			
			output[i][0]=-1;
			
			for (int j=0;j<mapSizeR;j++){
				chargeMapP[i][j][0]=0;
				chargeMapP[i][j][1]=0;
				chargeMapP[i][j][2]=0;
				
				polar2cartesianX[i][j]=((double)j*Math.cos( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
				polar2cartesianY[i][j]=((double)j*Math.sin( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
			}
		}
		
		counter=0;
	}
	
	//*********************************************************************
	// set sensor values
	//*********************************************************************
	public void touchEnvironment(double[] r,Color[] c, int act,float speed){
		
		//////////////////////////////////////////////////////
		// save previous values
		//////////////////////////////////////////////////////
		// pressure on each neuron
		for (int i=0;i<resolution*sensorRes;i++){
			m_tactilePressureOld[i]=m_tactilePressure[i];
		}
		// position of detected point for each sensor
		/*for (int i=0;i<resolution;i++){
			valueOldX[i]=valueX[i];
			valueOldY[i]=valueY[i];
		}*/
		
		
		
		////////////////////////////////////////////////////////
		// set sensors values
		////////////////////////////////////////////////////////
		// sensors around ernest
		senseAround(r,c);
		
		// sensors in front of ernest (to redefine)
		//senseFront(r,c);
		
		
		
		
		/////////////////////////////////////////////////////////
		// place neurons on the map
		/////////////////////////////////////////////////////////
		/*double dist,dist2,dist3;
		double a,b;
		
		float capacity=500; 
		
        // compute neuron "capacity"
        for (int i=0;i<resolution*sensorRes;i++){
                if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= capacity;
                else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-capacity;
                else{
                        if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
                        else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
                }
        }*/
        
		/*
        // compute relation between neurons
        for (int i=0;i<resolution*sensorRes;i++){
                for (int j=0;j<resolution*sensorRes;j++){
                        if (i!=j){
                                if ( (m_tactileVariations[i]== capacity && m_tactileVariations[j]>0) 
                                  || (m_tactileVariations[i]==-capacity && m_tactileVariations[j]<0) ){
                                        
                                        connections[i][j]= (connections[i][j]*connectConfidence[i][j]
                                                           + Math.abs(m_tactileVariations[j]))/(connectConfidence[i][j]+1);
                                        if (connectConfidence[i][j]<10000) connectConfidence[i][j]++;
                                }
                        }
                }
        }
		
        /*
		// change distance between neuron
		for (int k=0;k<5;k++){
			for (int i=0;i<resolution*sensorRes;i++){
				int i2= (int) (i/resolution);
				int i3= i-i2*resolution;
				for (int j=0;j<resolution*sensorRes;j++){
					int j2= (int) (j/resolution);
					int j3= j-j2*resolution;
					if (i!=j && i3!=j3 && connections[i][j]>200){
						dist2= (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]);
						dist = Math.sqrt(dist2);
						dist3=dist/5;
						
						sensorX[i]+= ((capacity-connections[i][j])/10-dist3)
						              *( (sensorX[i]-sensorX[j]) / dist )*attraction;
							
						sensorY[i]+= ((capacity-connections[i][j])/10-dist3)
						              *( (sensorY[i]-sensorY[j]) / dist )*attraction;
						
						sensorX[j]-= ((capacity-connections[i][j])/10-dist3)
			              *( (sensorX[i]-sensorX[j]) / dist )*attraction;
				
						sensorY[j]-= ((capacity-connections[i][j])/10-dist3)
			              *( (sensorY[i]-sensorY[j]) / dist )*attraction;
							
						//m_constraints[i][j]=(float) ((40-connections[i][j])*m_distances[i][j]-dist3);
						
					}
				}
			}
		}
		normalize();
		 */
		
        
        
		// reset maps
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				
				chargeMap0[i][j][0]=chargeMap1[i][j][0];
				chargeMap0[i][j][1]=chargeMap1[i][j][1];
				chargeMap0[i][j][2]=chargeMap1[i][j][2];
				chargeTestMap[i][j]=false;
			}
		}
        
        
        ///////////////////////////////////////////////////////////////
        // fill charge map
        ///////////////////////////////////////////////////////////////
        float scale=100/mapSize;
		double x,y;
		double dx,dy;
		float val=0;
		float value=0;
		int ix,jy;
        // fill the charge map
		for (int i=0;i<resolution*(sensorRes-1);i++){
			int res=5;
			if (i>=resolution*(sensorRes-2)) res=6;
			for (int j=0;j<res;j++){
				
				dx=(sensorX[i+resolution]-sensorX[i])/5;
				dy=(sensorY[i+resolution]-sensorY[i])/5;
					
				// /!\ only because difference between two sensor neurons is 5
				if (i<resolution) val=j;
				else if (i<resolution*2) val=j+5;
				else val=j+10;
					
				
				int i2= i%resolution;
				value=-1;
				if (m_tactileValue[i2]<=val+1 && m_tactileValue[i2]>=val-1) value=1;
				else if (m_tactileValue[i2]>val+1) value=0;
				
				//if (m_tactileValue[i2]<=val) value=1;
				//else value=0;

				if (j==0){
					x= Math.min(mapSize-1, Math.max(0, (sensorX[i]+mapSize/2)/scale));
					y= Math.min(mapSize-1, Math.max(0, (sensorY[i]+mapSize/2)/scale));
				}
				else if (j==5){
					x= Math.min(mapSize-1, Math.max(0, (sensorX[i+resolution]+mapSize/2)/scale));
					y= Math.min(mapSize-1, Math.max(0, (sensorY[i+resolution]+mapSize/2)/scale));
				}
				else{
					x= Math.min(mapSize-1, Math.max(0, (sensorX[i]+j*dx+mapSize/2)/scale));
					y= Math.min(mapSize-1, Math.max(0, (sensorY[i]+j*dy+mapSize/2)/scale));
				}
				
				ix=(int) Math.round(x);
				jy=(int) Math.round(y);

				if (x<mapSize-1 && x>0 && y<mapSize-1 && y>0){
					
					if (value>=0){
						if (value>0){
							if (m_tactileObject[i2].equals(new Color(0,128,0))){
								chargeMap1[ix][jy][1]=1;
								chargeMap1[ix][jy][2]=0;
								chargeMap1[ix][jy][0]=0;
							}
							else{
								chargeMap1[ix][jy][1]=0;
								chargeMap1[ix][jy][2]=1;
								chargeMap1[ix][jy][0]=0;
							}
						}
						else{
							chargeMap1[ix][jy][0]=1;
							chargeMap1[ix][jy][1]=0;
							chargeMap1[ix][jy][2]=0;
						}
						
						
						chargeTestMap[ix][jy]=true;
					}
				}
			}
		}
        
        
	}
	
	
	
	
	//********************************************************************
	// compute rotation and translation coefficients
	//********************************************************************
	public void coefficients(double[] r,Color[] c, int act,float speed){
		
		///////////////////////////////////////////////////////////////
        // add new flow map
        ///////////////////////////////////////////////////////////////
		if (flowX1.size()<act+1){
			while (flowX1.size()<act+1){
				flowX1.add(new float[mapSize][mapSize]);
				flowY1.add(new float[mapSize][mapSize]);
				flowX2.add(new float[mapSize][mapSize]);
				flowY2.add(new float[mapSize][mapSize]);
				confidenceFlow.add(new float[mapSize][mapSize]);
				
				flowVectorX.add(new float[resolution*sensorRes]);
				flowVectorY.add(new float[resolution*sensorRes]);
				vectorConfidence.add(new float[resolution*sensorRes]);
				
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
				
				for (int i=0;i<resolution*sensorRes;i++){
					flowVectorX.get(act)[i]=0;
					flowVectorY.get(act)[i]=0;
					vectorConfidence.get(act)[i]=0;
				}
			}
		}
		
		///////////////////////////////////////////////////////////////
		// compute neuron flow
		///////////////////////////////////////////////////////////////
		float capacity=500; 
		double d=0;
		double vect,vx,vy;
		double dt=0;
		for (int i=0;i<resolution*sensorRes;i++){
			for (int j=resolution;j<resolution*2;j++){
				if (i!=j){
					if ( (m_tactileVariations[i]== capacity && m_tactileVariations[j]>0) 
                      || (m_tactileVariations[i]==-capacity && m_tactileVariations[j]<0)
                      /*|| (m_tactileVariations[j]==-capacity && m_tactileVariations[i]<0)
                      || (m_tactileVariations[j]==-capacity && m_tactileVariations[i]<0)*/){
						
						d=Math.sqrt( (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j])
                                    +(sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]) );
                                    
						dt=Math.abs(m_tactileVariations[i])-Math.abs(m_tactileVariations[j]);
                                    
						if (dt!=0 && d>0 && d<20 && speed>1){
                                    
							vect= d/dt;
                                    
							vx= (1/speed)*vect * (sensorX[i]-sensorX[j])/d;
							vy= (1/speed)*vect * (sensorY[i]-sensorY[j])/d;

							flowVectorX.get(act)[j]=  (float) ((flowVectorX.get(act)[j]*vectorConfidence.get(act)[j] + vx)
                                    						 / (vectorConfidence.get(act)[j] +1));
							
							flowVectorY.get(act)[j]=  (float) ((flowVectorY.get(act)[j]*vectorConfidence.get(act)[j] + vy)
                                            				 / (vectorConfidence.get(act)[j] +1));
                                    
							if (vectorConfidence.get(act)[j]<100000) vectorConfidence.get(act)[j]++;
                                   
						}
						
					}

				}
            }
		}
		
		////////////////////////////////////////////////////////////////////////
		// compute average translation and rotation vectors
		////////////////////////////////////////////////////////////////////////
		// TODO

	}
	
	
	
	//*********************************************************************
	// move "charges" on the charge map and generate polar charge map
	//*********************************************************************
	public void moveCharges(double translationX,double translationY,double rotation,float speed){
		////////////////////////////////////////////////////////////////////////
		// move charges
		////////////////////////////////////////////////////////////////////////
		float mx=0;
		float my=0;
		double d=0;
		float countD=0;
		float chargeSum0=0;
		float chargeSum1=0;
		float chargeSum2=0;
		int ix,jy;
		int ixi2,jyj2;
		double flowX,flowY;
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				float imap=i-mapSize/2;
				float jmap=j-mapSize/2;
				
				// compute local movement vector
				flowX= (float)(imap*Math.cos(rotation))
                     - (float)(jmap*Math.sin(rotation));
                flowY= (float)(imap*Math.sin(rotation))
                     + (float)(jmap*Math.cos(rotation));
                      
                flowX-=imap;
                flowY-=jmap;
                
				flowX+=translationX;
				flowY+=translationY;
				
				if (!chargeTestMap[i][j]){
				
					mx=(float) ((float)i-flowX*speed);
					my=(float) ((float)j-flowY*speed);
			
					ix=Math.round(mx);
					jy=Math.round(my);
				
					chargeSum0=0;
					chargeSum1=0;
					chargeSum2=0;
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							
							ixi2=ix+i2;
							jyj2=jy+j2;
							
							d= ((float)(ixi2)-mx)*((float)(ixi2)-mx) 
						      +((float)(jyj2)-my)*((float)(jyj2)-my);
							d=Math.min(1,Math.sqrt(d));
							
							if (ixi2>=0 && ixi2<mapSize && jyj2>=0 && jyj2<mapSize){
								chargeSum0+=chargeMap0[ixi2][jyj2][0]*(1-d);
								chargeSum1+=chargeMap0[ixi2][jyj2][1]*(1-d);
								chargeSum2+=chargeMap0[ixi2][jyj2][2]*(1-d);
								countD+=(1-d);
							}
							else{
								countD+=(1-d);
							}
						}
					}
				
					if (countD>0){
						chargeSum0=chargeSum0/countD;
						chargeSum1=chargeSum1/countD;
						chargeSum2=chargeSum2/countD;

						chargeMap1[i][j][0]=(float) Math.min(1,chargeSum0) ;
						chargeMap1[i][j][1]=(float) Math.min(1,chargeSum1) ;
						chargeMap1[i][j][2]=(float) Math.min(1,chargeSum2) ;
					}
				}
			}
		}
		
		////////////////////////////////////////////////////////////////////////
		// generate polar map
		////////////////////////////////////////////////////////////////////////
		
		double Sum0,Sum1,Sum2;
		double px,py;
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				px=polar2cartesianX[i][j];
				py=polar2cartesianY[i][j];
				
				ix=(int) Math.round(px);
				jy=(int) Math.round(py);
				
				if (ix>=0 && jy>=0 && ix<mapSize && jy<mapSize){
					
					Sum0=0;
					Sum1=0;
					Sum2=0;
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							ixi2=ix+i2;
							jyj2=jy+j2;
							if (ixi2>=0 && ixi2<mapSize && jyj2>=0 && jyj2<mapSize){
								d= ((float)(ixi2)-px)*((float)(ixi2)-px) 
								  +((float)(jyj2)-py)*((float)(jyj2)-py);
								d=Math.min(1,Math.sqrt(d));
								Sum0+=chargeMap0[ixi2][jyj2][0]*(1-d);
								Sum1+=chargeMap0[ixi2][jyj2][1]*(1-d);
								Sum2+=chargeMap0[ixi2][jyj2][2]*(1-d);
								countD+=(1-d);
							}
						}
					}
					
					chargeMapP[i][j][0]=Math.min(1,(float)(Sum0/countD));
					chargeMapP[i][j][1]=Math.min(1,(float)(Sum1/countD));
					chargeMapP[i][j][2]=Math.min(1,(float)(Sum2/countD));
				}
			}
		}
		
		/*
		////////////////////////////////////////////////////////////////////////
		// detection of high probability area
		////////////////////////////////////////////////////////////////////////
		float max=(float) 0.1;
		int index=-1;
		for (int i=1;i<mapSizeTheta-1;i++){
			for (int j=1;j<mapSizeR-1;j++){
				max=(float) 0.1;
				index=-1;
				maximumMap[i][j]=-1;
				for (int k=0;k<3;k++){
					
					if (chargeMapP[i][j][k]>max){
						max=chargeMapP[i][j][k];
						index=k;
					}
				}
				if (index>=0){
					if (    chargeMapP[i][j][index] >= chargeMapP[i][j-1][index]
					     && chargeMapP[i][j][index] >= chargeMapP[i][j+1][index]){
						
						maximumMap[i][j]=index;
					}
				}
			}
		}*/
		
		
		////////////////////////////////////////////////////////////////////////
		// fill the output vector
		////////////////////////////////////////////////////////////////////////
		boolean test;						// first maximum found
		int j;
		float max=0;						// value of the first maximum
		int index=0;						// index of the first maximum
		int position=-1;
		
		for (int i=0;i<mapSizeTheta;i++){
			
			index=0;
			position=-1;
			max=0;
			
			
			// find value, position and index of first maximum
			j=0;
			test=false;
			while (!test && j<mapSizeR-1){
				for (int k=1;k<3;k++){
					if (    chargeMapP[i][j][k]>=0.99 
						|| (chargeMapP[i][j][k]>=0.1  && chargeMapP[i][j+1][k]<chargeMapP[i][j][k]) ){
						test=true;
						max=chargeMapP[i][j][k];
						index=k;
						position=j;
					}
				}
				j++;
			}
			
			output[i][0]=index;
			output[i][1]=position;
			output[i][2]=position;
			output[i][3]=position;
			
			// if maximum found, find the area of the object
			if (test){
				
				j=0;
				boolean test2=false;
				while (!test2 && position-j>=0){
					
					if (chargeMapP[i][position-j][index]<=max*0.3){
						test2=true;
					}
					
					j++;
				}
				output[i][2]=position-j;
				
				j=0;
				test2=false;
				while (!test2 && position+j<mapSizeR){
					
					if (chargeMapP[i][position+j][index]<=max*0.3){
						test2=true;
					}
					
					j++;
				}
				output[i][3]=position+j;
				
			}
			
		}

	}
	
	/*
	public void touchEnvironment2(double[] r,Color[] c, int act,float speed){
		

		// set potential and charge Map values
		float scale=200/mapSize;
		double x,y;
		double dx,dy;
		float val=0;
		float value=0;
		int ix,jy;

		
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (chargeTestMap[i][j]){
					chargeMap1[i][j][0]=potentialMap2[i][j][0];
					chargeMap1[i][j][1]=potentialMap2[i][j][1];
					chargeMap1[i][j][2]=potentialMap2[i][j][2];
				}			
			}
		}*/
		
		/*
		// compute flow
		float fx,fy;
		int l=1;
		for (int i=l;i<mapSize-l;i++){
			for (int j=l;j<mapSize-l;j++){
				
				boolean test1=true;
				boolean test2=true;
				
				d=Math.sqrt( (double)((i-25)*(i-25) + (j-25)*(j-25) ));
				
				if (Math.abs(potentialMapOld[i-l][j]-potentialMapOld[i+l][j])>0.005
						&& Math.abs(potentialMapOld[i][j]-potentialMapOld[i+l][j])>0.001
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i-l][j])>0.001
						  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMapOld[i+l][j]<=potentialMapOld[i][j])
							 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMapOld[i+l][j]>=potentialMapOld[i][j]) )
 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i+l][j]>0 && potentialMap[i+l][j]<1 && potentialMapOld[i+l][j]>0 && potentialMapOld[i+l][j]<1
						  && potentialMap[i-l][j]>0 && potentialMap[i-l][j]<1 && potentialMapOld[i-l][j]>0 && potentialMapOld[i-l][j]<1
						  
						  && potentialTestMap[i][j] && potentialTestMap[i-l][j] 
						  && potentialTestMap[i+l][j] && potentialTestMap[i][j-l] && potentialTestMap[i][j+l]
						  && speed>1){
					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]) );
					
				}
				else{
					test1=false;
					fx=0;
				}
				
				if (Math.abs(potentialMapOld[i][j-l]-potentialMapOld[i][j+l])>0.005
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j+l])>0.001
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j-l])>0.001
						  && ( ( potentialMapOld[i][j-l]>=potentialMapOld[i][j] && potentialMapOld[i][j+l]<=potentialMapOld[i][j])
							 ||( potentialMapOld[i][j-l]<=potentialMapOld[i][j] && potentialMapOld[i][j+l]>=potentialMapOld[i][j]) )
						
							 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i][j+l]>0 && potentialMap[i][j+l]<1 && potentialMapOld[i][j+l]>0 && potentialMapOld[i][j+l]<1
						  && potentialMap[i][j-l]>0 && potentialMap[i][j-l]<1 && potentialMapOld[i][j-l]>0 && potentialMapOld[i][j-l]<1
						  
						  && potentialTestMap[i][j] && potentialTestMap[i-l][j] && potentialTestMap[i+l][j] 
						  && potentialTestMap[i][j-l] && potentialTestMap[i][j+l]
						  && speed>1){
					
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]) );
					
				}
				else{
					test2=true;
					fy=0;
				}
				
				if (test1 && test2){
					
					if (fx!=0) flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
					if (fy!=0) flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
				
					if (confidenceFlow.get(act)[i][j]<50000) confidenceFlow.get(act)[i][j]++;
				}
				
			}
		}
		
		// reduce noise
		int count;
		float mx,my;
		int s=5;
		int k=act;
		int i3,j3;

		for (int i=s;i<mapSize-s;i++){
				for (int j=s;j<mapSize-s;j++){
					if (potentialTestMap[i][j]){
						flowX2.get(k)[i][j]=0;
						flowY2.get(k)[i][j]=0;
						count=0;
						mx=0;
						my=0;
						for (int i2=-s;i2<=s;i2++){
						for (int j2=-s;j2<=s;j2++){
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
		}*/

		
			
		
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
		for (int i=s;i<mapSize-s;i++){
			for (int j=s;j<mapSize-s;j++){
				if (potentialTestMap[i][j] && confidenceFlow.get(act)[i][j]>10){
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
		if (count>0){
			mTranslationX.set(act, mx/(float)count);
			mTranslationY.set(act, my/(float)count);
			mRotation.set(act, mTheta/(float)count);
		}
		
		// set the extrapolated flow field
		fx=0;
		fy=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (!potentialTestMap[i][j]){
					
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
					for (int i2=-4;i2<4;i2++){
						for (int j2=-4;j2<4;j2++){
							if ( (i+i2)>=0 && (i+i2)<mapSize && (j+j2)>=0 && (j+j2)<mapSize){
								if (potentialTestMap[i+i2][j+j2]){
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

	}*/


	public void senseAround(double[] r,Color[] c){
		float distance,distance2;
		int angle=360/resolution;
		
		int E_angle=ernest.m_orientation+540;
		int index=0;
		for (int i=0;i<360;i+=angle){
			distance=(float) r[(i+E_angle)%360];
			for (int j=0;j<sensorRes;j++){
				
				index=i/angle;
				m_tactileValue[index]=Math.min(16, distance-5);
				m_tactileObject[index]=c[(i+E_angle)%360];
				
				if (j==0){
					index=i/angle;
					if (distance<=5){
						distance2=1;
						//distance2= Math.min(1,1- (distance-5)/5);
						m_tactilePressure[index]= distance2;
					}
					else{
						m_tactilePressure[index]=0;
					}
				}
				else if (j==1){
					index=i/angle+resolution;
					if (distance<=10){
						distance2=1;
						//distance2=(float) Math.min(1,1- (distance-7)/5);
						m_tactilePressure[index]= distance2;
					}
					else{
						m_tactilePressure[index]=0;
					}
				}
				else{
					index=i/angle+2*resolution;
					if (distance<=15){
						distance2=1;
						//distance2=(float) Math.min(1,1- (distance-10)/5);
						m_tactilePressure[index]= distance2;
					}
					else{
						m_tactilePressure[index]=0;
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
			distance=(float) r[(i+E_angle)%360];
			for (int j=0;j<sensorRes;j++){
				if (j==0){
					if (distance<=10){
						//distance2=1;
						distance2= Math.min(1,1- (distance-5)/5);
						m_tactilePressure[i/angle]= distance2;
						m_tactileObject[i/angle]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[i/angle]=0;
						m_tactileObject[i/angle]=Color.black;
					}
				}
				else if (j==1){
					if (distance<=12){
						//distance2=1;
						distance2=(float) Math.min(1,1- (distance-7)/5);
						m_tactilePressure[i/angle+resolution]= distance2;
						m_tactileObject[i/angle+resolution]=c[(i+E_angle)%360];
					}
					else{
						m_tactilePressure[i/angle+resolution]=0;
						m_tactileObject[i/angle+resolution]=Color.black;
					}
				}
				else{
					if (distance<=15){
						//distance2=1;
						distance2=(float) Math.min(1,1- (distance-10)/5);
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
		double angle= Math.sin(sensorX[resolution/2]/d);
		
		for (int i=0;i<resolution*sensorRes;i++){
			sensorX[i]= sensorX[i]*Math.cos(-angle) - sensorY[i]*Math.sin(-angle);
			sensorY[i]= sensorX[i]*Math.sin(-angle) + sensorY[i]*Math.cos(-angle);
		}
	}
	
	
}