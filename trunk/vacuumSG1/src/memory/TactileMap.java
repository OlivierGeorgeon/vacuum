package memory;
import java.awt.Color;
import java.util.ArrayList;
import agent.*;

/**
 * tactile colliculus
 * @author simon
 */
public class TactileMap {

	public int[] m_tactilePressure;               // value of each sensor neurons
	public int[] m_tactilePressureOld;
	
	public float[] m_tactileValue;                  // value of sensors
	
	public float[][] m_constraints;
	public double[][] m_distances;
	public float[][] m_connectionsLenght;
	
	public float[] m_tactileVariations;				// neurons "capacity"
	
	public double[][] connections;					// average distance between neurons
	public double[][] connectConfidence;
	
	public int [] timerMap;
	public int lastAction;
	
	//public ArrayList<float[][]> speedDirection;
	//public ArrayList<int[][]> speedDirectionConfidence;
	//public ArrayList<float[]> speedDirectionX;
	//public ArrayList<float[]> speedDirectionY;
	
	//public float   actionLink[][][];
	//public int confidenceLink[][][];
	//public float actionValue;
	
	public float voronoiValue[][];
	public int voronoiPoints[][];
	
	public boolean delaunayLinks[][];
	
	public float previousState[][][];
	
	public ArrayList<float[]> flowVectorX;			// movement vector on each neuron
	public ArrayList<float[]> flowVectorY;
	public ArrayList<float[]> vectorConfidence;

	public int[] m_tactileObject;
	public double[] sensorX;                        // position of sensor neurons
	public double[] sensorY;
	
	public double[] neutralX;                        // position of neutral neurons
	public double[] neutralY;
	
	public double[] valueX;							// position of the detected point
	public double[] valueY;
	public double[] valueOldX;
	public double[] valueOldY;
	public double attraction;
	int nbStimuli=5;
	
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
	

	/**
	 * initialization of parameters
	 */
	private void initialize(){
		mapSize=100;
		mapSizeTheta=180;
		mapSizeR=100;
		
		m_tactileValue=new float[resolution];
		m_tactilePressure=new int[resolution*sensorRes];
		m_tactilePressureOld=new int[resolution*sensorRes];
		m_tactileVariations=new float[resolution*sensorRes];
		flowVectorX=new ArrayList<float[]>();
		flowVectorY=new ArrayList<float[]>();
		vectorConfidence=new ArrayList<float[]>();
		
		timerMap=new int[resolution*sensorRes];
		
		//speedDirection=new ArrayList<float[][]>();
		//speedDirectionConfidence=new ArrayList<int[][]>();
		//speedDirectionX=new ArrayList<float[]>();
		//speedDirectionY=new ArrayList<float[]>();
		//actionLink  =new float[resolution*sensorRes][resolution*sensorRes][3];
		//confidenceLink=new int[resolution*sensorRes][resolution*sensorRes][3];
		
		voronoiValue=new float[100][100];
		voronoiPoints=new int[100][100];
		delaunayLinks=new boolean[resolution*sensorRes+20][resolution*sensorRes+20];
		
		previousState=new float[resolution*sensorRes][resolution*sensorRes][150];
		
		m_tactileObject=new int[resolution];
		m_constraints=new float[resolution*sensorRes][resolution*sensorRes];
		m_distances=new double[resolution*sensorRes][resolution*sensorRes];
		m_connectionsLenght=new float[resolution*sensorRes][resolution*sensorRes];
		connections=new double[resolution*sensorRes][resolution*sensorRes];
		connectConfidence =new double[resolution*sensorRes][resolution*sensorRes];
		sensorX=new double[resolution*sensorRes];
		sensorY=new double[resolution*sensorRes];
		
		neutralX=new double[20];
		neutralY=new double[20];
		
		valueX=new double[resolution];
		valueY=new double[resolution];
		valueOldX=new double[resolution];
		valueOldY=new double[resolution];
		attraction=0.001;
		
		
		chargeMap0=new float[mapSize][mapSize][nbStimuli];
		chargeMap1=new float[mapSize][mapSize][nbStimuli];
		chargeMapP=new float[mapSizeTheta][mapSizeR][nbStimuli];
		maximumMap=new int[mapSizeTheta][mapSizeR];
		output=new int[mapSizeTheta][nbStimuli];
		
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
				
				for (int k=0;k<150;k++){
					previousState[i][j][k]=0;
				}
			}
			
			// initialize random neurons positions
			sensorX[i]= (float) (Math.random()*40-20);//-50*Math.sin(360/resolution*sensorRes*i*Math.PI/180);
			sensorY[i]= (float) (Math.random()*40-20);// 50*Math.cos(360/resolution*sensorRes*i*Math.PI/180);
			
			
		}
		
		// initialize neutral neurons
		for (int i=0;i<20;i++){
			neutralX[i]= -30*Math.sin(360/20*i*Math.PI/180);
			neutralY[i]=  30*Math.cos(360/20*i*Math.PI/180);
		}
		
		/*
		// initialize real neuron position
		for (int j=0;j<sensorRes;j++){
			for (int i=0;i<resolution;i++){
				float r=0;
				if (j==0) r=5;
				if (j==1) r=10;
				if (j==2) r=15;
				sensorX[i+j*resolution]= -(r)*Math.sin(360/resolution*i*Math.PI/180);
				sensorY[i+j*resolution]=  (r)*Math.cos(360/resolution*i*Math.PI/180);
			}
		}/**/
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j][0]=0;
				chargeMap0[i][j][1]=0;
				chargeMap0[i][j][2]=0;
				chargeMap0[i][j][3]=0;
				
				chargeMap1[i][j][0]=0;
				chargeMap1[i][j][1]=0;
				chargeMap1[i][j][2]=0;
				chargeMap1[i][j][3]=0;
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
				chargeMapP[i][j][3]=0;
				
				polar2cartesianX[i][j]=((double)j*Math.cos( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
				polar2cartesianY[i][j]=((double)j*Math.sin( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
			}
		}
		
		counter=0;
		
		
		voronoi();
		delaunay();
	}
	
	/**
	 * read sensor values, map sensors and fill the potential map
	 */
	public void touchEnvironment(double[] rt,int[] t){
		
		//////////////////////////////////////////////////////
		// save previous values
		//////////////////////////////////////////////////////
		// pressure on each neuron
		boolean changed=false;;
		for (int i=0;i<resolution*sensorRes;i++){	
			m_tactilePressureOld[i]=m_tactilePressure[i];
		}
		// position of detected point for each sensor
		for (int i=0;i<resolution;i++){
			valueOldX[i]=valueX[i];
			valueOldY[i]=valueY[i];
		}/**/
		
		//voronoi();
		
		////////////////////////////////////////////////////////
		// set sensors values
		////////////////////////////////////////////////////////
		// sensors around ernest
		senseAround(rt,t);
		
		// detect wrong points
		boolean test=false;
		for (int i=0;i<resolution*sensorRes;i++){
			
			/*
			// if point changed its state
			if (m_tactilePressureOld[i]!=m_tactilePressure[i]){
				// Localize the closest point with the same state
				double dmin=1000;
				int min=-1;
				double d=0;
				for (int j=0;j<resolution*sensorRes;j++){
					if (i!=j){
						if (  (m_tactilePressure[i]!=0 && m_tactilePressure[j]!=0)
							||(m_tactilePressure[i]==0 && m_tactilePressure[j]==0) ){
							
							d=Math.sqrt( (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]) );
							
							if (d<dmin){
								dmin=d;
								min=j;
							}
						}
					}
				}
				
				if (min>=0){
					sensorX[i]-=(sensorX[i]-sensorX[min])/2;	
					sensorY[i]-=(sensorY[i]-sensorY[min])/2;
				}
			}/**/
		}
		
		voronoi();
		delaunay();
		
		
		/////////////////////////////////////////////////////////
		// place neurons on the map
		/////////////////////////////////////////////////////////
		double dist,dist2,dist3;
		double a,b;
		
		float capacity=500; 
		/*
        // compute neuron "capacity" (asymmetric version)
        for (int i=0;i<resolution*sensorRes;i++){
                if (m_tactilePressure[i] > m_tactilePressureOld[i])      m_tactileVariations[i]= capacity;
                else if (m_tactilePressure[i] < m_tactilePressureOld[i]) m_tactileVariations[i]=-capacity;
                else{
                        if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
                        else if (m_tactileVariations[i] < 0) m_tactileVariations[i]++;
                }
        } 
        
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
        }/* */
        
		/*
       	// compute neuron "capacity" (symmetric version)
        for (int i=0;i<resolution*sensorRes;i++){
            if (m_tactilePressure[i] != m_tactilePressureOld[i])      m_tactileVariations[i]= capacity;
            else{
            	if (m_tactileVariations[i] > 0) m_tactileVariations[i]--;
            }
        }
        
        // compute relation between neurons
        for (int i=0;i<resolution*sensorRes;i++){
                for (int j=0;j<resolution*sensorRes;j++){
                        if (i!=j){
                                if ( (m_tactileVariations[i]== capacity && m_tactileVariations[j]!=0) ){
                                        
                                        connections[i][j]= (connections[i][j]*connectConfidence[i][j]
                                                           + m_tactileVariations[j])/(connectConfidence[i][j]+1);
                                        if (connectConfidence[i][j]<10000) connectConfidence[i][j]++;
                                }
                        }
                }
        }/**/
		
        /*
		// change distance between neuron
		for (int k=0;k<5;k++){
			for (int i=0;i<resolution*sensorRes;i++){
				int i2= (int) (i/resolution);
				int i3= i-i2*resolution;
				for (int j=0;j<resolution*sensorRes;j++){
					int j2= (int) (j/resolution);
					int j3= j-j2*resolution;
					if (i!=j  ){
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
		/* */
		
        // set timers
		for (int i=0;i<resolution*sensorRes;i++){
			if (timerMap[i]>0) timerMap[i]--;
			
			if (m_tactilePressure[i]!=m_tactilePressureOld[i])
				timerMap[i]=20;
		}
		
		repulsion();
		
		////////////////////////////////////////////////////////////////
		// reset maps
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				
				chargeMap0[i][j][0]=chargeMap1[i][j][0];
				chargeMap0[i][j][1]=chargeMap1[i][j][1];
				chargeMap0[i][j][2]=chargeMap1[i][j][2];
				chargeMap0[i][j][3]=chargeMap1[i][j][3];
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
							if (   m_tactileObject[i2]==3){
								chargeMap1[ix][jy][0]=0;
								chargeMap1[ix][jy][1]=1;
								chargeMap1[ix][jy][2]=0;
								chargeMap1[ix][jy][3]=0;
							}
							else if (m_tactileObject[i2]==1 ){
								chargeMap1[ix][jy][0]=0;
								chargeMap1[ix][jy][1]=0;
								chargeMap1[ix][jy][2]=1;
								chargeMap1[ix][jy][3]=0;
							}
							else{
								chargeMap1[ix][jy][0]=0;
								chargeMap1[ix][jy][1]=0;
								chargeMap1[ix][jy][2]=0;
								chargeMap1[ix][jy][3]=1;
							}
						}
						else{
							chargeMap1[ix][jy][0]=1;
							chargeMap1[ix][jy][1]=0;
							chargeMap1[ix][jy][2]=0;
							chargeMap1[ix][jy][3]=0;
						}
						
						
						chargeTestMap[ix][jy]=true;
					}
					else{
						chargeMap1[ix][jy][0]=0;
						chargeMap1[ix][jy][1]=0;
						chargeMap1[ix][jy][2]=0;
						chargeMap1[ix][jy][3]=0;
					}
				}
			}
		}
        
        
	}
	
	


	/**
	 * compute rotation and translation coefficients
	 * @param act	actual action performed by the agent
	 * @param speed	value of the action
	 */
	public void coefficients(int act,float speed){
		
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
				
				//speedDirection.add(new float[resolution*sensorRes][resolution*sensorRes]);
				//speedDirectionConfidence.add(new int[resolution*sensorRes][resolution*sensorRes]);
				//speedDirectionX.add(new float[resolution*sensorRes]);
				//speedDirectionY.add(new float[resolution*sensorRes]);
				
				mTranslationX.add((float) 0);
				mTranslationY.add((float) 0);
				mRotation.add((float) 0);
				
				for (int i=0;i<mapSize;i++){
					for (int j=0;j<mapSize;j++){
						flowX1.get(flowX1.size()-1)[i][j]=0;
						flowY1.get(flowX1.size()-1)[i][j]=0;
						flowX2.get(flowX1.size()-1)[i][j]=0;
						flowY2.get(flowX1.size()-1)[i][j]=0;
						confidenceFlow.get(flowX1.size()-1)[i][j]=0;
						for (int k=0;k<flowLength;k++){
							flowLineX1.get(flowX1.size()-1)[i][j][k]=0;
							flowLineY1.get(flowX1.size()-1)[i][j][k]=0;
							flowLineX2.get(flowX1.size()-1)[i][j][k]=0;
							flowLineY2.get(flowX1.size()-1)[i][j][k]=0;
						}
					}
				}
				
				for (int i=0;i<resolution*sensorRes;i++){
					flowVectorX.get(flowX1.size()-1)[i]=0;
					flowVectorY.get(flowX1.size()-1)[i]=0;
					vectorConfidence.get(flowX1.size()-1)[i]=0;
					//speedDirectionX.get(flowX1.size()-1)[i]=0;
					//speedDirectionY.get(flowX1.size()-1)[i]=0;
					/*for (int j=0;j<resolution*sensorRes;j++){
						speedDirection.get(flowX1.size()-1)[i][j]=0;
						speedDirectionConfidence.get(flowX1.size()-1)[i][j]=0;
					}*/
				}
			}
		}
		
		
		// reset timer map if action change
		if (act!=lastAction){
			/*for (int i=0;i<resolution*sensorRes;i++){
				timerMap[i]=0;
			}/**/
			lastAction=act;
		}
		
		//actionValue=speed;
		
		/*
		///////////////////////////////////////////////////////////////
		// compute action links
		///////////////////////////////////////////////////////////////
		for (int i=0;i<resolution*sensorRes;i++){
			if (timerMap[i]==20){
				for (int j=0;j<resolution*sensorRes;j++){
					if (i!=j){
						if (timerMap[j]>0){
							actionLink[i][j][act]= (actionLink[i][j][act]*(float)confidenceLink[i][j][act] + 1 )
							                      /( (float)confidenceLink[i][j][act] + 1 );
						}
						else{
							actionLink[i][j][act]= (actionLink[i][j][act]*(float)confidenceLink[i][j][act]     )
											      /( (float)confidenceLink[i][j][act] + 1 );
						}
						
						if (confidenceLink[i][j][act]<100) confidenceLink[i][j][act]++;
					}
				}
			}
		} /* */
		
		
		
		///////////////////////////////////////////////////////////////
		// compute previous state
		///////////////////////////////////////////////////////////////
		/*for (int i=0;i<resolution*sensorRes;i++){
			for (int j=0;j<resolution*sensorRes;j++){
				
				if (timerMap[i]==20){
					if (timerMap[j]==20){
						previousState[i][j][lastAction*50+(int)(actionValue*30)]=
							(previousState[i][j][lastAction*50+(int)(actionValue*30)]*100+1)/101;
					}
					else{
						previousState[i][j][lastAction*50+(int)(actionValue*30)]=
							(previousState[i][j][lastAction*50+(int)(actionValue*30)]*100-1)/101;
					}
					
				}
				else{
					if (timerMap[j]==20){
						previousState[i][j][lastAction*50+(int)(actionValue*30)]=
							(previousState[i][j][lastAction*50+(int)(actionValue*30)]*100-1)/101;
					}
					else{
						previousState[i][j][lastAction*50+(int)(actionValue*30)]=
							(previousState[i][j][lastAction*50+(int)(actionValue*30)]*100+1)/101;
					}
				}
				
			}
		}/**/
		
		
		
		/*
		///////////////////////////////////////////////////////////////
		// compute speed vector direction
		///////////////////////////////////////////////////////////////
		float sumX=0;
		float sumY=0;
		for (int i=0;i<resolution*sensorRes;i++){
			sumX=0;
			sumY=0;
			if (timerMap[i]==20){
				
				for (int j=0;j<resolution*sensorRes;j++){
					if (i!=j){
						if (timerMap[j]>0){
							speedDirection.get(act)[i][j]= 
								( speedDirection.get(act)[i][j]*(float)speedDirectionConfidence.get(act)[i][j] + 1 )
								/ ((float)speedDirectionConfidence.get(act)[i][j] + 1 );
						}
						else{
							speedDirection.get(act)[i][j]= 
								( speedDirection.get(act)[i][j]*(float)speedDirectionConfidence.get(act)[i][j] )
								/ ((float)speedDirectionConfidence.get(act)[i][j] + 1 );
						}
						speedDirectionConfidence.get(act)[i][j]++;
						
						sumX+=((sensorX[i]-sensorX[j])/Math.abs(sensorX[i]-sensorX[j]))*speedDirection.get(act)[i][j];
						sumY+=((sensorY[i]-sensorY[j])/Math.abs(sensorY[i]-sensorY[j]))*speedDirection.get(act)[i][j];
					}
						
				}
				speedDirectionX.get(act)[i]=sumX/(resolution*sensorRes-1);
				speedDirectionY.get(act)[i]=sumY/(resolution*sensorRes-1);
			}
		}/**/
		
		
		/*
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
                      || (m_tactileVariations[j]==-capacity && m_tactileVariations[i]<0)*//*){
						
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
		*/
	}
	
	

	/**
	 * move "charges" on the charge map and generate polar charge map
	 * @param translationX	Average translation component on X  axis, given by the global colliculus
	 * @param translationY	Average translation component on Y  axis, given by the global colliculus
	 * @param rotation		Average rotation value, given by the global colliculus
	 * @param speed			Value of the current action
	 */
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
		float chargeSum3=0;
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
					chargeSum3=0;
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
								chargeSum3+=chargeMap0[ixi2][jyj2][3]*(1-d);
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
						chargeSum3=chargeSum3/countD;

						chargeMap1[i][j][0]=(float) Math.min(1,chargeSum0) ;
						chargeMap1[i][j][1]=(float) Math.min(1,chargeSum1) ;
						chargeMap1[i][j][2]=(float) Math.min(1,chargeSum2) ;
						chargeMap1[i][j][3]=(float) Math.min(1,chargeSum3) ;
					}
				}
			}
		}
		
		////////////////////////////////////////////////////////////////////////
		// generate polar map
		////////////////////////////////////////////////////////////////////////
		
		double Sum0,Sum1,Sum2,Sum3;
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
					Sum3=0;
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
								Sum3+=chargeMap0[ixi2][jyj2][3]*(1-d);
								countD+=(1-d);
							}
						}
					}
					
					chargeMapP[i][j][0]=Math.min(1,(float)(Sum0/countD));
					chargeMapP[i][j][1]=Math.min(1,(float)(Sum1/countD));
					chargeMapP[i][j][2]=Math.min(1,(float)(Sum2/countD));
					chargeMapP[i][j][3]=Math.min(1,(float)(Sum3/countD));
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
				for (int k=1;k<nbStimuli;k++){
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


	/**
	 * read sensors when distance sensor are divided around Ernest
	 */
	public void senseAround(double[] rt,int[] t){
		float distance,distance2;
		int angle=360/resolution;
		
		int index=0;
		for (int i=0;i<360;i+=angle){
			distance=(float) rt[(i+360)%360];
			for (int j=0;j<sensorRes;j++){
				
				index=i/angle;
				m_tactileValue[index]=Math.min(16, distance-5);
				m_tactileObject[index]=t[(i+360)%360];
				
				if (j==0){
					index=i/angle;
					if (distance<=6){
						distance2=1;
						//distance2= Math.min(1,1- (distance-5)/5);
						m_tactilePressure[index]= t[(i)%360];
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
						m_tactilePressure[index]= t[(i)%360];
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
						m_tactilePressure[index]= t[(i)%360];;
					}
					else{
						m_tactilePressure[index]=0;
					}
				}
			}
		}
	}
	
	
	
	
	
	public void voronoi(){
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				voronoiValue[i][j]=0;
				voronoiPoints[i][j]=-1;
			}
		}
		double d=0;
		int x,y;
		for (int i=0;i<resolution*sensorRes;i++){
			x=(int)(2*sensorX[i])+50;
			y=(int)(2*sensorY[i])+50;
			for (int i2=-20;i2<20;i2++){
				for (int j2=-20;j2<20;j2++){
					if ( (i2!=0 || j2!=0) && x+i2 >=0 && x+i2<100 && y+j2 >=0 && y+j2<100){
						d=1/Math.sqrt(i2*i2+j2*j2);
						
						if (d>voronoiValue[x+i2][y+j2]){
							voronoiValue[x+i2][y+j2]=(float)d;
							voronoiPoints[x+i2][y+j2]=i;
						}
						
					}
					if (i2==0 && j2==0 && x >=0 && x<100 && y >=0 && y<100){
						voronoiValue[x][y]=1;
						voronoiPoints[x][y]=i;
					}
				}
			}
		}
		
		for (int i=0;i<20;i++){
			x=(int)(2*neutralX[i])+50;
			y=(int)(2*neutralY[i])+50;
			for (int i2=-20;i2<20;i2++){
				for (int j2=-20;j2<20;j2++){
					if ( (i2!=0 || j2!=0) && x+i2 >=0 && x+i2<100 && y+j2 >=0 && y+j2<100){
						d=1/Math.sqrt(i2*i2+j2*j2);
						
						if (d>voronoiValue[x+i2][y+j2]){
							voronoiValue[x+i2][y+j2]=(float)d;
							voronoiPoints[x+i2][y+j2]=i+resolution*sensorRes;
						}
						
					}
					if (i2==0 && j2==0 && x >=0 && x<100 && y >=0 && y<100){
						voronoiValue[x][y]=1;
						voronoiPoints[x][y]=i+resolution*sensorRes;
					}
				}
			}
		}
	}
	
	
	public void delaunay(){
		for (int i=0;i<resolution*sensorRes+20;i++){
			for (int j=0;j<resolution*sensorRes+20;j++){
				delaunayLinks[i][j]=false;
			}
		}
		
		for (int i=0;i<99;i++){
			for (int j=0;j<99;j++){
				if (voronoiPoints[i][j]!=-1){
					if (voronoiPoints[i][j]!=voronoiPoints[i+1][j] && voronoiPoints[i+1][j]!=-1){
						delaunayLinks[voronoiPoints[i][j]][voronoiPoints[i+1][j]]=true;
					}
					if (voronoiPoints[i][j]!=voronoiPoints[i][j+1] && voronoiPoints[i][j+1]!=-1){
						delaunayLinks[voronoiPoints[i][j]][voronoiPoints[i][j+1]]=true;
					}
				}
			}
		}
	}
	
	
	public void repulsion(){
		for (int i=0;i<resolution*sensorRes;i++){
			for (int j=0;j<resolution*sensorRes;j++){
				double d=Math.sqrt( (sensorX[i]-sensorX[j])*(sensorX[i]-sensorX[j]) + (sensorY[i]-sensorY[j])*(sensorY[i]-sensorY[j]) );
				if (d<3 && d!=0){
					sensorX[i]+=(sensorX[i]-sensorX[j])/d;
					sensorY[i]+=(sensorY[i]-sensorY[j])/d;
				}
				
			}
		}
	}
	
	/**
	 * normalize position and angle of the global sensor network
	 */
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

	/*
	/////////////////////////////////////////////////////////////////////////////
	public class TactileArray{
		int act;
		int value;
		int[] t_array;
		
		float[] nextArray;
		
		public TactileArray(int a, int v,int[] array){
			act=a;
			value=v;
			t_array=array;
			
			nextArray=new float[array.length];
			for (int i=0;i<array.length;i++){
				nextArray[i]=0.5f;
			}
		}
		
		public boolean isEqual(TactileArray t){
			boolean test=false;
			int i=0;
			if (this.t_array.length==t.t_array.length && this.act==t.act && this.value==t.value){
				test=true;
				while (i<t_array.length && test){
					if (t_array[i]!=t.t_array[i]) test=false;
					i++;
				}
			}
			return test;
		}
		
		public void compute(int[] array){
			for (int i=0;i<t_array.length;i++){
				nextArray[i] = (nextArray[i] *100 + (float)array[i]) /101;
			}
		}
	}*/
	
	
	
}