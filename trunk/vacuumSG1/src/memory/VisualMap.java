package memory;
import java.awt.Color;
import java.util.ArrayList;
import agent.*;

/**
 * Visual colliculus
 * @author simon
 */
public class VisualMap {

	public Color[][] colorMap;
	public Color[][] colorMapOld;
	public float[][] potentialMap;
	public float[][] potentialMapReduced1;
	public float[][] potentialMapReduced2;
	public float[][] potentialMapOld;
	public int[][] timerMap;
	
	public ArrayList<float[][]> speedDirection;
	public ArrayList<int[][]> speedDirectionConfidence;
	public ArrayList<float[][]> speedDirectionX;
	public ArrayList<float[][]> speedDirectionY;
	
	public ArrayList<float[][]> speedDirectionX2;
	public ArrayList<float[][]> speedDirectionY2;
	
	public int blobMap[][];
	public int blobIndex;
	public ArrayList<int[]> pixelList;
	
	public float chargeMap0[][][];
	public float chargeMap1[][][];
	public float chargeMapP[][][];
	public int maximumMap[][];
	public int output[][];
	private boolean[][] chargeTestMap;
	public float[][] confidenceMap;
	public float[][] potentialConfidenceMap;
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
	public ArrayList<float[][]> flowX3;				// flow in Cartesian referential
	public ArrayList<float[][]> flowY3;
	public ArrayList<float[][]> confidenceFlow;
	public ArrayList<float[][]> confidenceFlowC;
	
	public ArrayList<Float> mTranslationX;
	public ArrayList<Float> mTranslationY;
	public ArrayList<Float> mRotation;
	
	public double polar2cartesianX[][];
	public double polar2cartesianY[][];
	
	public double polar2cartesian2X[][];
	public double polar2cartesian2Y[][];
	
	public double cartesian2polarR[][];
	public double cartesian2polarT[][];
	
	public ErnestModel ernest;
	public int mapSize,mapSizeTheta,mapSizeR;
	
	
	public VisualMap(ErnestModel e){
		ernest=e;
		mapSize=100;
		mapSizeTheta=180;
		mapSizeR=100;
		
		colorMap=new Color[mapSizeTheta][mapSizeR];
		colorMapOld=new Color[mapSizeTheta][mapSizeR];
		potentialMap=new float[mapSizeTheta][mapSizeR];
		potentialMapReduced1=new float[mapSizeTheta][mapSizeR];
		potentialMapReduced2=new float[mapSizeTheta][mapSizeR];
		potentialMapOld=new float[mapSizeTheta][mapSizeR];
		timerMap=new int[mapSizeTheta][mapSizeR];
		chargeMap0=new float[mapSize][mapSize][11];
		chargeMap1=new float[mapSize][mapSize][11];
		chargeMapP=new float[mapSizeTheta][mapSizeR][11];
		
		blobMap=new int[mapSizeTheta][mapSizeR];
		pixelList=new ArrayList<int[]>();
		
		maximumMap=new int[mapSizeTheta][mapSizeR];
		output=new int[mapSizeTheta][4];
		
		chargeTestMap=new boolean[mapSize][mapSize];
		confidenceMap=new float[mapSizeTheta][mapSizeR];
		potentialConfidenceMap=new float[mapSize][mapSize];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
		flowX3=new ArrayList<float[][]>();
		flowY3=new ArrayList<float[][]>();
		
		speedDirection=new ArrayList<float[][]>();
		speedDirectionConfidence=new ArrayList<int[][]>();
		speedDirectionX=new ArrayList<float[][]>();
		speedDirectionY=new ArrayList<float[][]>();
		speedDirectionX2=new ArrayList<float[][]>();
		speedDirectionY2=new ArrayList<float[][]>();
		confidenceFlow=new ArrayList<float[][]>();
		confidenceFlowC=new ArrayList<float[][]>();
		
		polar2cartesianX=new double[mapSizeTheta][mapSizeR];
		polar2cartesianY=new double[mapSizeTheta][mapSizeR];
		polar2cartesian2X=new double[mapSizeTheta][mapSizeR];
		polar2cartesian2Y=new double[mapSizeTheta][mapSizeR];
		cartesian2polarR=new double[mapSize][mapSize];
		cartesian2polarT=new double[mapSize][mapSize];
		
		mTranslationX=new ArrayList<Float>();
		mTranslationY=new ArrayList<Float>();
		mRotation    =new ArrayList<Float>();
		
		for (int i=0;i<180;i++){
			for (int j=0;j<mapSizeR;j++){
				confidenceMap[i][j]=-1;
				for (int k=0;k<10;k++){
					chargeMapP[i][j][k]=0;
				}
				
				colorMap[i][j]=Color.black;
				colorMapOld[i][j]=Color.black;
				timerMap[i][j]=0;
				
				polar2cartesianX[i][j]=((double)j*Math.cos( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
				polar2cartesianY[i][j]=((double)j*Math.sin( ((double)(i*2+90))*Math.PI/180))+mapSize/2;

				polar2cartesian2X[i][j]=( (float)j * Math.cos((float)i*Math.PI/180) );
				polar2cartesian2Y[i][j]=( (float)j * Math.sin((float)i*Math.PI/180) );
			}
		}
		
		
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				
				for (int k=0;k<10;k++){
					chargeMap0[i][j][k]=0;
					chargeMap1[i][j][k]=0;
				}
				potentialConfidenceMap[i][j]=0;
				
				cartesian2polarR[i][j]= Math.sqrt( (float)((i-mapSize/2)*(i-mapSize/2) + (j-mapSize/2)*(j-mapSize/2)) );
				
				double theta0=0;
				if (j-mapSize/2 <0) theta0=(float)( Math.atan( -((float)i-mapSize/2)/((float)j-mapSize/2) )*180/Math.PI);
				else if (j-mapSize/2==0 && i-mapSize/2>0) theta0= 90;
				else if (j-mapSize/2==0 && i-mapSize/2<0) theta0=-90;
						
				cartesian2polarT[i][j]=theta0+90;
			}
		}
		
	}
	
	
	
	/**
	 * set sensor values, fill charge map in polar and Cartesian referential
	 * @param r		Distance vector
	 * @param c		Color vector
	 */
	public void seeEnvironment(double[] r,Color[] c){
		// reset maps and save previous values
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				potentialMapOld[i][j]=potentialMap[i][j];
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				for (int k=0;k<10;k++){
					chargeMap0[i][j][k]=chargeMap1[i][j][k];
				}
				chargeTestMap[i][j]=false;
			}
		}
		
		
		///////////////////////////////////////////////////////
		// set colors on polar map
		///////////////////////////////////////////////////////
		for (int i=0;i<mapSizeTheta;i++){
			
			//int confidence=Math.min(40, Math.abs(i-90)/2+1);
			int min=(int) r[i+90]-1;//(int) Math.max(0, r[i]-confidence/2);
			int max=(int) r[i+90]+5;//(int) Math.min(mapSizeR, r[i]+confidence/2+1);

			for (int j=10;j<mapSizeR;j++){

				colorMapOld[i][j]=colorMap[i][j];
				
				if (j>=min && j<max){
					colorMap[i][j]=c[i+90];
					confidenceMap[i][j]=5;
				}
				else{
					if (j<min){
						colorMap[i][j]=Color.black;
						confidenceMap[i][j]=5;
					}
					else{
						colorMap[i][j]=Color.black;//null;
						confidenceMap[i][j]=-1;
					}
				}
				
			}
			
		}
		
		
		///////////////////////////////////////////////////////
		// fill potential map (polar)
		///////////////////////////////////////////////////////
		double sum;
		double counter;
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				counter=0;
				sum=0;

				if (confidenceMap[i][j]>=0 && !colorMap[i][j].equals(Color.black)){
					potentialMap[i][j]=1;
				}else{
					potentialMap[i][j]=0;
				}
			}
		}
		
		double theta0,r0;
		
		
		///////////////////////////////////////////////////////
		// reduction of potential map
		///////////////////////////////////////////////////////
		
		/*
		// Initialization
		int count=0;
		int ii2,jj2;
		for (int i=2;i<mapSizeTheta-2;i++){
			for (int j=2;j<mapSizeR-2;j++){
				count=0;
				if (potentialMap[i][j]==1){
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							ii2=i+i2*2;
							jj2=j+j2*2;
							if ( (ii2!=0 || jj2!=0) && potentialMap[ii2][jj2]==1) count++; 
						}
					}
					if (count>6) potentialMapReduced1[i][j]=1;
					else potentialMapReduced1[i][j]=0;
				}
				else potentialMapReduced1[i][j]=0;
			}
		}
		
		for (int i=0;i<mapSizeTheta;i+=2){
			for (int j=0;j<mapSizeR;j+=2){
				potentialMapReduced2[i][j]=0;
			}
		}*/
		/*
		///////////////////////////////////////////////////////
		// fill blob map (Polar)
		///////////////////////////////////////////////////////
		blobIndex=0;
		pixelList.clear();
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				blobMap[i][j]=-1;
			}
		}
		
		int px,py;
		int mx,my;
		int sumBlob;
		
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				if (timerMap[i][j]>0) timerMap[i][j]--;
				
				if (blobMap[i][j]==-1 && potentialMapReduced1[i][j]==1){
					blobMap[i][j]=blobIndex;
					
					mx=0;
					my=0;
					sumBlob=0;
					
					mx+=i;
					my+=j;
					sumBlob++;
					
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							ii2=i+i2*2;
							jj2=j+j2*2;
							if ( (i2!=0 || j2!=0) && ii2>=0 && ii2<mapSizeTheta && jj2>=0 && jj2<mapSizeR){
								if (blobMap[ii2][jj2]==-1 && potentialMapReduced1[ii2][jj2]==1){
									blobMap[ii2][jj2]=blobIndex;
									int[] pixel={ii2,jj2};
									pixelList.add( pixel );
								}
							}
						}
					}
					
					while (!pixelList.isEmpty()){
						px=pixelList.get(0)[0];
						py=pixelList.get(0)[1];
						
						mx+=px;
						my+=py;
						sumBlob++;
						
						pixelList.remove(0);
						
						for (int i2=-1;i2<=1;i2++){
							for (int j2=-1;j2<=1;j2++){
								ii2=px+i2*2;
								jj2=py+j2*2;
								if ( (i2!=0 || j2!=0) && ii2>=0 && ii2<mapSizeTheta && jj2>=0 && jj2<mapSizeR){
									if (blobMap[ii2][jj2]==-1 && potentialMapReduced1[ii2][jj2]==1){
										blobMap[ii2][jj2]=blobIndex;
										int[] pixel={ii2,jj2};
										pixelList.add( pixel );
									}
								}
							}
						}
					}
					
					blobIndex++;
					
					mx=Math.round((float)mx/(float)sumBlob);
					my=Math.round((float)my/(float)sumBlob);
					
					potentialMapReduced2[mx][my]=1;
					
					timerMap[i][j]=10;
				}
			}
		}*/
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				if (timerMap[i][j]>0) timerMap[i][j]--;
				
				if (!colorMap[i][j].equals(colorMapOld[i][j]))
					timerMap[i][j]=10;
			}
		}
		
		///////////////////////////////////////////////////////
		// fill charge map (Cartesian)
		///////////////////////////////////////////////////////
		double sum0[];
		sum0=new double[10];
		double count0,d;
		int x2i2,y2j2;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize/2+1;j++){
				
					
				r0=cartesian2polarR[i][j];
				
				if (j-mapSize/2<=0){
					theta0=cartesian2polarT[i][j];
					
					int x2=(int) Math.round(r0);
					int y2=(int) Math.round(theta0);
					
					for (int k=0;k<10;k++){
						sum0[k]=0;
					}
					count0=0;
					for (int i2=-2;i2<=2;i2++){
						for (int j2=-2;j2<=2;j2++){
							
							x2i2=x2+i2;
							y2j2=y2+j2;
							
							if (x2i2>=0 && x2i2<mapSizeR && y2j2>=0 && y2j2<mapSizeTheta){
								d= ((float)(x2i2)-r0    )*((float)(x2i2)-r0    ) 
							      +((float)(y2j2)-theta0)*((float)(y2j2)-theta0);
								d=Math.min(1,Math.sqrt(d));
								
								if (confidenceMap[y2j2][x2i2]>=0){
									if (colorMap[y2j2][x2i2].equals(Color.black)){
										sum0[0]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(0,128,0)) ){
										sum0[1]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(115,230,0)) ){
										sum0[2]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(150,128,255)) ){
										sum0[3]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(46,230,0)) ){
										sum0[4]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(0,230,230)) ){
										sum0[5]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(0,230,92)) ){
										sum0[6]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(230,207,0)) ){
										sum0[7]+= 1-d;
									}
									else if (colorMap[y2j2][x2i2].equals(new Color(0,230,161)) ){
										sum0[8]+= 1-d;
									}
									else{
										sum0[9]+= 1-d;
									}
									
									count0+=1-d;
								}
							}
						}
					}
					
					if (count0>0){
						for (int k=0;k<10;k++){
							chargeMap1[i][j][k]=(float) (sum0[k]/count0);
						}
						chargeTestMap[i][j]=true;
					}
					else chargeTestMap[i][j]=false;
				}

			}
		}
	}
	
	
	/**
	 * compute average t and r coefficients
	 * @param act	current action performed by the agent
	 * @param speed	value of the action
	 */
	public void coefficients(int act,float speed){
		
		// add new flow map
		if (flowX1.size()<act+1){
			while (flowX1.size()<act+1){
				flowX1.add(new float[mapSizeTheta][mapSizeR]);
				flowY1.add(new float[mapSizeTheta][mapSizeR]);
				flowX2.add(new float[mapSizeTheta][mapSizeR]);
				flowY2.add(new float[mapSizeTheta][mapSizeR]);
				flowX3.add(new float[mapSize][mapSize]);
				flowY3.add(new float[mapSize][mapSize]);
				confidenceFlow.add(new float[mapSizeTheta][mapSizeR]);
				confidenceFlowC.add(new float[mapSize][mapSize]);
				
				speedDirection.add(new float[mapSizeTheta][mapSizeR]);
				speedDirectionConfidence.add(new int[mapSizeTheta][mapSizeR]);
				speedDirectionX.add(new float[mapSizeTheta][mapSizeR]);
				speedDirectionY.add(new float[mapSizeTheta][mapSizeR]);
				speedDirectionX2.add(new float[mapSizeTheta][mapSizeR]);
				speedDirectionY2.add(new float[mapSizeTheta][mapSizeR]);
				
				mTranslationX.add((float) 0);
				mTranslationY.add((float) 0);
				mRotation.add((float) 0);
				
				for (int i=0;i<mapSizeTheta;i++){
					for (int j=0;j<mapSizeR;j++){
						flowX1.get(act)[i][j]=0;
						flowY1.get(act)[i][j]=0;
						flowX2.get(act)[i][j]=0;
						flowY2.get(act)[i][j]=0;
						confidenceFlow.get(act)[i][j]=0;
						speedDirection.get(act)[i][j]=0;
					}
				}
				for (int i=0;i<mapSize;i++){
					for (int j=0;j<mapSize;j++){
						flowX3.get(act)[i][j]=0;
						flowY3.get(act)[i][j]=0;
						confidenceFlowC.get(act)[i][j]=0;
					}
				}
			}
		}
		
		float sumX=0;
		float sumY=0;
		for (int i=5;i<180;i+=5){
			for (int j=5;j<100;j+=5){
				sumX=0;
				sumY=0;
				if (timerMap[i][j]==10){

					for (int i2=-2;i2<=2;i2++){
						for (int j2=-2;j2<=2;j2++){
							if (i!=0 && j!=0){
								if (timerMap[i+i2][j+j2]>0){
									speedDirection.get(act)[i+i2][j+j2]= 
										( speedDirection.get(act)[i+i2][j+j2]*(float)speedDirectionConfidence.get(act)[i+i2][j+j2] + 1 )
										/ ((float)speedDirectionConfidence.get(act)[i+i2][j+j2] + 1 );
								}
								else{
									speedDirection.get(act)[i+i2][j+j2]= 
										( speedDirection.get(act)[i+i2][j+j2]*(float)speedDirectionConfidence.get(act)[i+i2][j+j2] )
										/ ((float)speedDirectionConfidence.get(act)[i+i2][j+j2] + 1 );
								}
								speedDirectionConfidence.get(act)[i+i2][j+j2]++;
							
								sumX+=i2*speedDirection.get(act)[i+i2][j+j2];
								sumY+=j2*speedDirection.get(act)[i+i2][j+j2];
							}
						}
					}
					speedDirectionX.get(act)[i][j]=sumX/24;
					speedDirectionY.get(act)[i][j]=sumY/24;
				}
			}
		}
		
		for (int i=10;i<175;i+=5){
			for (int j=10;j<95;j+=5){
				sumX=0;
				sumY=0;
				for (int i2=-5;i2<=5;i2+=5){
					for (int j2=-5;j2<=5;j2+=5){
						if (i!=0 && j!=0){
							sumX+=speedDirectionX.get(act)[i+i2][j+j2];
							sumY+=speedDirectionY.get(act)[i+i2][j+j2];
						}
					}
				}
				speedDirectionX2.get(act)[i][j]=sumX/8;
				speedDirectionY2.get(act)[i][j]=sumY/8;
			}
		}
		
		/*
		// compute flow
		double d;
		int ii2,jj2;
		float fx,fy;
		int l=1;
		
		int confidenceX=0,confidenceY=0;
		for (int i=2*l;i<180-2*l;i+=2){
			for (int j=2*l;j<100-2*l;j+=2){
				if (timerMap[i][j]==10 && speed>0.1){
					
					confidenceX=0;
					confidenceY=0;
					fx=0;
					fy=0;
					for (int i2=-l;i2<=l;i2++){
						for (int j2=-l;j2<=l;j2++){
							
							if (i2!=0 || j2!=0){
								ii2=i+i2*2;
								jj2=j+j2*2;
								if (timerMap[ii2][jj2]>0 && timerMap[ii2][jj2]<10){
									//d=Math.sqrt(i2*i2+j2*j2);
									
									if (i2!=0){
										fx-=(1/speed) * i2/(10-timerMap[ii2][jj2]);
										confidenceX++;
									}
									if (j2!=0){
										fy+=(1/speed) * j2/(10-timerMap[ii2][jj2]);
										confidenceY++;
									}
								}
							}
							
							
						}
					}
					
					
					
					if (confidenceX>0 || confidenceY>0){
		
						if (confidenceX==0)confidenceX=1;
						if (confidenceY==0)confidenceY=1;
						
						flowX1.get(act)[i][j]=(float)( ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx/confidenceX)
						                           	 / (confidenceFlow.get(act)[i][j]+1)  );
						flowY1.get(act)[i][j]=(float)( ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy/confidenceY)
	                           	 / (confidenceFlow.get(act)[i][j]+1)  );
						if (confidenceFlow.get(act)[i][j]<10000) confidenceFlow.get(act)[i][j]++;
					}

				}
			}
		} /* */

		/*
		////////////////////////////////////////////////////////////////////////
		// reduce noise
		////////////////////////////////////////////////////////////////////////
		int count;
		float mx,my;
		int k=act;
		int i3,j3;
		for (int i=10;i<mapSizeTheta-10;i+=2){
			for (int j=10;j<mapSizeR-10;j+=2){
				if (flowX1.get(k)[i][j]!=0 || flowY1.get(k)[i][j]!=0){
					flowX2.get(k)[i][j]=0;
					flowY2.get(k)[i][j]=0;
					count=0;
					mx=0;
					my=0;
					for (int i2=-1;i2<=1;i2++){
					for (int j2=-1;j2<=1;j2++){
						i3=i+i2*2;
						j3=j+j2*2;
						if (flowX1.get(k)[i3][j3]!=0 || flowY1.get(k)[i3][j3]!=0){
							count++;
							mx+=flowX1.get(k)[i3][j3];
							my+=flowY1.get(k)[i3][j3];
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
		} /* */
		

		
		/*
		////////////////////////////////////////////////////////////////////////
		// compute average translation and rotation vectors
		////////////////////////////////////////////////////////////////////////
		
		count=0;
		float mTheta=0;
		float mx2,my2;
		float mx3=0;
		float my3=0;
		int a=2;
		float translationX=0,translationY=0;
		for (int i=a;i<mapSizeTheta-a;i+=2){
			for (int j=a;j<mapSizeR-a;j+=2){
				
				if (confidenceFlow.get(act)[i][j]>0){
					// rotation
					mTheta-=flowX2.get(k)[i][j];
					
					// translation
					//mx= (float) polar2cartesian2X[i][j];
					//my= (float) polar2cartesian2Y[i][j];
					
					mx= (float) ((float)j/2* Math.cos((float)(i) *Math.PI/180 ));
					my= (float) ((float)j/2* Math.sin((float)(i) *Math.PI/180));
					

					mx2= (float) ( ((float)j+flowY2.get(k)[i][j]) /2
						* Math.cos( ((float)i-flowX2.get(k)[i][j])*Math.PI/180) );
					my2= (float) ( ((float)j+flowY2.get(k)[i][j]) /2
						* Math.sin( ((float)i-flowX2.get(k)[i][j])*Math.PI/180) );
					/*
					if ( Math.round(mx+50)>0 && Math.round(mx+50)<mapSize && Math.round(my+50)>0 && Math.round(my+50)<mapSize){
						flowX3.get(act)[Math.round(mx+50)][Math.round(my+50)]=mx-mx2;
						flowY3.get(act)[Math.round(mx+50)][Math.round(my+50)]=my-my2;
					}*/
					/*
					mx3+=mx2-mx;
					my3+=my2-my;

					count++;
				}
				
			}
		}
		
		translationX= mx3/(float)count;
		translationY= my3/(float)count;

		
		if (count>0){
			//if (act==0){
				mTranslationX.set(act, mx3/(float)count/2);
				mTranslationY.set(act, my3/(float)count/2);
			//}else{
				mRotation.set(act, (float)((mTheta/(float)count)*Math.PI/180));
			//}
		}
		
		
		// fill cartesian flow map
        fx=0;
        fy=0;
        float imap,jmap;
        for (int i=0;i<mapSize;i++){
                for (int j=0;j<mapSize;j++){
                               
                	imap=(float)(i-mapSize/2);
                    jmap=(float)(j-mapSize/2);
                	
                                fx= (float)(imap*Math.cos(mRotation.get(act)) )
                                  - (float)(jmap*Math.sin(mRotation.get(act)) );
                                fy= (float)(imap*Math.sin(mRotation.get(act)) ) 
                                  + (float)(jmap*Math.cos(mRotation.get(act)) );
                                
                                fx-=imap;
                                fy-=jmap;
                                
                                flowX3.get(act)[i][j]=fx +mTranslationX.get(act);
                                flowY3.get(act)[i][j]=fy +mTranslationY.get(act);
                }
        } /* */
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
		float fx=0;
		float fy=0;
		double flowX=0;
		double flowY=0;
		float mx=0;
		float my=0;
		double d=0;
		float countD=0;
		float chargeSum0[];
		float imap,jmap;
		chargeSum0=new float[10];
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				imap=i-mapSize/2;
                jmap=j-mapSize/2;
				// compute local movement vector
				if (!chargeTestMap[i][j]){
					fx= (float)(imap*Math.cos(rotation) )
					  - (float)(jmap*Math.sin(rotation) );
					fy= (float)(imap*Math.sin(rotation) )
					  + (float)(jmap*Math.cos(rotation) );
					
					fx-=imap;
					fy-=jmap;
					
					flowX=fx +translationX;
					flowY=fy +translationY;

				
					mx=(float)i+(float)(flowX*speed);
					my=(float)j+(float)(flowY*speed);
			
					int ix=Math.round(mx);
					int jy=Math.round(my);
				
					for (int k=0;k<10;k++){
						chargeSum0[k]=0;
					}
					
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							d= ((float)(ix+i2)-mx)*((float)(ix+i2)-mx) 
						      +((float)(jy+j2)-my)*((float)(jy+j2)-my);
							d=(float) Math.min(1,Math.sqrt(d));
							if (ix+i2>=0 && ix+i2<mapSize && jy+j2>=0 && jy+j2<mapSize){
								for (int k=0;k<10;k++){
									chargeSum0[k]+=chargeMap0[ix+i2][jy+j2][k]*(1-d);
								}
								countD+=(1-d);
							}
							else{
								countD+=(1-d);
							}
						}
					}
				
					if (countD>0 ){
						for (int k=0;k<10;k++){
							chargeSum0[k]=chargeSum0[k]/countD;
							chargeMap1[i][j][k]=(float) Math.min(1,chargeSum0[k]);
						}
					}
				}

			}
		}
		
		
		////////////////////////////////////////////////////////////////////////
		// generate polar map
		////////////////////////////////////////////////////////////////////////
		
		double Sum0[];
		Sum0=new double[10];
		double px,py;
		int ix,jy;
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				px=polar2cartesianX[i][j];
				py=polar2cartesianY[i][j];;
				
				ix=(int) Math.round(px);
				jy=(int) Math.round(py);
				
				if (ix>=0 && jy>=0 && ix<mapSize && jy<mapSize){
					
					for (int k=0;k<10;k++){
						Sum0[k]=0;
					}
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (ix+i2>=0 && ix+i2<mapSize && jy+j2>=0 && jy+j2<mapSize){
								d= ((float)(ix+i2)-px)*((float)(ix+i2)-px) 
								  +((float)(jy+j2)-py)*((float)(jy+j2)-py);
								d=Math.min(1,Math.sqrt(d));
								for (int k=0;k<10;k++){
									Sum0[k]+=chargeMap0[ix+i2][jy+j2][k]*(1-d);
								}
								countD+=(1-d);
							}
						}
					}
					for (int k=0;k<10;k++){
						chargeMapP[i][j][k]=Math.min(1,(float)(Sum0[k]/countD));
					}
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
				for (int k=0;k<10;k++){
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
		boolean test;							// first maximum found
		int j;
		float max=0;					// value of the first maximum
		int index=0;							// index of the first maximum
		int position=-1;
		
		for (int i=0;i<mapSizeTheta;i++){
			
			index=0;
			position=-1;
			max=0;
			
			
			// find value, position and index of first maximum
			j=0;
			test=false;
			while (!test && j<mapSizeR-1){
				for (int k=1;k<10;k++){
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
					
					if (chargeMapP[i][position-j][index]<=max/2){
						test2=true;
					}
					
					j++;
				}
				output[i][2]=position-j;
				
				j=0;
				test2=false;
				while (!test2 && position+j<mapSizeR){
					
					if (chargeMapP[i][position+j][index]<=max/2){
						test2=true;
					}
					
					j++;
				}
				output[i][3]=position+j;
				
			}
			
		}
		
	}
	

	
	
}