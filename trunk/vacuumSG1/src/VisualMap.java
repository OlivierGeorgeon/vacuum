import java.awt.Color;
import java.util.ArrayList;



public class VisualMap {

	public Color[][] colorMap;
	public float[][] potentialMap;
	public float[][] potentialMapOld;
	public float chargeMap0[][];
	public float chargeMap1[][];
	public float[][] confidenceMap;
	public float[][] potentialConfidenceMap;
	public boolean testMap[][];
	public boolean testMapP[][];
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
	public ArrayList<float[][]> flowX3;				// flow in Cartesian referential
	public ArrayList<float[][]> flowY3;
	public ArrayList<float[][]> confidenceFlow;
	public ArrayList<Float> mTranslationX;
	public ArrayList<Float> mTranslationY;
	public ArrayList<Float> mRotation;
	
	public ErnestModel ernest;
	public int mapSize,mapSizeTheta,mapSizeR;
	
	public VisualMap(ErnestModel e){
		ernest=e;
		mapSize=50;
		mapSizeTheta=180;
		mapSizeR=100;
		colorMap=new Color[mapSizeTheta][mapSizeR];
		potentialMap=new float[mapSizeTheta][mapSizeR];
		potentialMapOld=new float[mapSizeTheta][mapSizeR];
		chargeMap0=new float[mapSize][mapSize];
		chargeMap1=new float[mapSize][mapSize];
		confidenceMap=new float[mapSizeTheta][mapSizeR];
		potentialConfidenceMap=new float[mapSize][mapSize];
		testMap=new boolean[mapSize][mapSize];
		testMapP=new boolean[mapSizeTheta][mapSizeR];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
		flowX3=new ArrayList<float[][]>();
		flowY3=new ArrayList<float[][]>();
		confidenceFlow=new ArrayList<float[][]>();
		
		mTranslationX=new ArrayList<Float>();
		mTranslationY=new ArrayList<Float>();
		mRotation    =new ArrayList<Float>();
		
		for (int i=0;i<180;i++){
			for (int j=0;j<mapSizeR;j++){
				confidenceMap[i][j]=-1;
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMap[i][j]=0;
				potentialMapOld[i][j]=0;
				chargeMap0[i][j]=0;
				chargeMap1[i][j]=0;
				potentialConfidenceMap[i][j]=0;
			}
		}
		
	}
	
	
	public void seeEnvironment(double[] r,Color[] c,int[] corners, int act,float speed){
		
		
		
		// reset maps
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				potentialMapOld[i][j]=potentialMap[i][j];
				testMapP[i][j]=false;
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				testMap[i][j]=false;
				chargeMap1[i][j]=0;
			}
		}
		
		
		// set colors on polar map
		for (int i=0;i<mapSizeTheta;i++){
			
			int confidence=Math.min(40, Math.abs(i-90)/2+1);
			int min=(int) r[i]-1;//(int) Math.max(0, r[i]-confidence/2);
			int max=(int) r[i]+5;//(int) Math.min(mapSizeR, r[i]+confidence/2+1);

			for (int j=0;j<mapSizeR;j++){

				if (j>=min && j<max){
					colorMap[i][j]=c[i];
					confidenceMap[i][j]=5;
				}
				else{
					colorMap[i][j]=null;
					confidenceMap[i][j]=-1;
				}
				
			}
			
			for (int j=0;j<Math.min(100,max);j++){
				testMapP[i][j]=true;
			}
			
		}

		
		
		///////////////////////////////////////////////////////
		// compute flow
		///////////////////////////////////////////////////////
		
		

		
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
					}
				}
				for (int i=0;i<mapSize;i++){
					for (int j=0;j<mapSize;j++){
						flowX3.get(act)[i][j]=0;
						flowY3.get(act)[i][j]=0;
					}
				}
			}
		}
		
		// fill potentialMap
		double sum;
		double counter;
		for (int i=0;i<mapSizeTheta;i++){
			for (int j=0;j<mapSizeR;j++){
				
				counter=0;
				sum=0;
				
				for (int i2=-2;i2<=2;i2++){
					for (int j2=-2;j2<=2;j2++){
						if (i+i2>=0 && i+i2<mapSizeTheta && j+j2>=0 && j+j2<mapSizeR){
							if (confidenceMap[i+i2][j+j2]>=0){
								sum+=1;
								counter+=1;
							}
							else{
								counter+=1;
							}
						}
					}
				}
				potentialMap[i][j]=(float) (sum/counter);
				
				/*if (confidenceMap[i][j]>=0){
					potentialMap[i][j]=1;
				}
				else{
					potentialMap[i][j]=0;
				}*/
				
			}
		}
		
		
		// fill charge map
		double theta0,r0;
		double sum1,count1,d;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize/2+1;j++){
				
					
				r0= (float) Math.sqrt( (float)((i-25)*(i-25) + (j-25)*(j-25)) );
				
				if (j-25<=0){
					theta0=0;
					if (j-25 <0) theta0=(float)( Math.atan( -((float)i-25)/((float)j-25) )*180/Math.PI);
					else if (j-25==0 && i-25>0) theta0= 90;
					else if (j-25==0 && i-25<0) theta0=-90;
						
					r0=r0*2;
					theta0=theta0+90;
					
					int x2=(int) Math.round(r0);
					int y2=(int) Math.round(theta0);
					
					sum1=0;
					count1=0;
					for (int i2=-2;i2<=2;i2++){
						for (int j2=-2;j2<=2;j2++){
							if (x2+i2>=0 && x2+i2<mapSizeR && y2+j2>=0 && y2+j2<mapSizeTheta){
								d= ((float)(x2+i2)-r0    )*((float)(x2+i2)-r0    ) 
							      +((float)(y2+j2)-theta0)*((float)(y2+j2)-theta0);
								d=Math.min(1,Math.sqrt(d));
								
								sum1+=potentialMap[y2+j2][x2+i2]*(1-d);
								count1+=(1-d);
							}
						}
					}
					
					if (count1>0){
						chargeMap1[i][j]=(float) (sum1/count1);
					}
					
					if (x2>=0 && x2<mapSizeR && y2>=0 && y2<mapSizeTheta){
						testMap[i][j]=testMapP[y2][x2];
					}
				}

			}
		}
		
		
		
		// compute flow
		float fx,fy;
		int l=1;
		boolean test1,test2;
		for (int i=l;i<mapSizeTheta-l;i++){
			for (int j=l;j<20;j++){
				test1=true;
				test2=true;
				if (( ( potentialMapOld[i-l][j]>potentialMapOld[i][j] && potentialMapOld[i+l][j]<potentialMapOld[i][j])
							 ||( potentialMapOld[i-l][j]<potentialMapOld[i][j] && potentialMapOld[i+l][j]>potentialMapOld[i][j]) )
 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i+l][j]>0 && potentialMap[i+l][j]<1 && potentialMapOld[i+l][j]>0 && potentialMapOld[i+l][j]<1
						  && potentialMap[i-l][j]>0 && potentialMap[i-l][j]<1 && potentialMapOld[i-l][j]>0 && potentialMapOld[i-l][j]<1
						  
						  && testMapP[i][j] && testMapP[i-l][j] && testMapP[i+l][j] && testMapP[i][j-l] && testMapP[i][j+l]
						  && speed>1){
					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]) );
					
				}
				else{
					test1=false;
					fx=0;
				}
				
				if (( ( potentialMapOld[i][j-l]>potentialMapOld[i][j] && potentialMapOld[i][j+l]<potentialMapOld[i][j])
							 ||( potentialMapOld[i][j-l]<potentialMapOld[i][j] && potentialMapOld[i][j+l]>potentialMapOld[i][j]) )
						
							 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i][j+l]>0 && potentialMap[i][j+l]<1 && potentialMapOld[i][j+l]>0 && potentialMapOld[i][j+l]<1
						  && potentialMap[i][j-l]>0 && potentialMap[i][j-l]<1 && potentialMapOld[i][j-l]>0 && potentialMapOld[i][j-l]<1
						  
						  && testMapP[i][j] && testMapP[i-l][j] && testMapP[i+l][j] && testMapP[i][j-l] && testMapP[i][j+l]
						  && speed>1){
					
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]) );
					
				}
				else{
					test2=false;
					fy=0;
				}
				
				if (test1 && test2){
					
					if (fx!=0) flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
					if (fy!=0) flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
				
					confidenceFlow.get(act)[i][j]++;
				}

			}
		}


		// reduce noise
		int count;
		float mx,my;
		int a=4;
		int k=act;
		int i3,j3;
		for (int i=a;i<mapSizeTheta-a;i++){
			for (int j=a;j<mapSizeR-a;j++){
				if (flowX1.get(k)[i][j]!=0 && flowY1.get(k)[i][j]!=0){
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
		}
		
		
		////////////////////////////////////////////////////////////////////////
		// compute average translation and rotation vectors
		////////////////////////////////////////////////////////////////////////
		
		count=0;
		float mTheta=0;
		float mx2,my2;
		float mx3=0;
		float my3=0;
		for (int i=a;i<mapSizeTheta-a;i++){
			for (int j=a;j<mapSizeR-a;j++){
				
				if (confidenceFlow.get(act)[i][j]>0){
					// rotation
					mTheta-=flowX2.get(k)[i][j];
					
					// translation
					mx= (float) ( (float)j * Math.cos((float)i*Math.PI/180) );
					my= (float) ( (float)j * Math.sin((float)i*Math.PI/180) );
					
					mx2= (float) ( ((float)j+flowY2.get(k)[i][j]) 
						* Math.cos( ((float)i+flowX2.get(k)[i][j])*Math.PI/180) );
					my2= (float) ( ((float)j+flowY2.get(k)[i][j]) 
						* Math.sin( ((float)i+flowX2.get(k)[i][j])*Math.PI/180) );
					
					mx3+=mx2-mx;
					my3+=my2-my;
					
					count++;
				}
				
			}
		}
		if (count>0){
			if (act==0){
				mTranslationX.set(act, mx3/(float)count/2);
				mTranslationY.set(act, my3/(float)count/2);
			}else{
				mRotation.set(act, (float)((mTheta/(float)count)*Math.PI/180));
			}
		}
		
		// fill cartesian flow map
		fx=0;
		fy=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
					
					fx= (float) ((float)(i-mapSize/2)*Math.cos(mRotation.get(act)) 
					  - (float)(j-mapSize/2)*Math.sin(mRotation.get(act)));
					fy= (float) ((float)(i-mapSize/2)*Math.sin(mRotation.get(act)) 
					  + (float)(j-mapSize/2)*Math.cos(mRotation.get(act)));
					
					fx-=(float)(i-mapSize/2);
					fy-=(float)(j-mapSize/2);
					
					flowX3.get(act)[i][j]=fx +mTranslationX.get(act);
					flowY3.get(act)[i][j]=fy +mTranslationY.get(act);
			}
		}
		
		////////////////////////////////////////////////////////////////////////
		// move charges
		////////////////////////////////////////////////////////////////////////
		
		mx=my=0;
		d=0;
		float countD=0;
		float chargeSum0=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (!testMap[i][j]){
					mx=(float)i+flowX3.get(act)[i][j]*speed;
					my=(float)j+flowY3.get(act)[i][j]*speed;
			
					int ix=Math.round(mx);
					int jy=Math.round(my);
				
					chargeSum0=0;
					countD=0;
					boolean test=true;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (ix+i2>=0 && ix+i2<mapSize && jy+j2>=0 && jy+j2<mapSize){
								d= ((float)(ix+i2)-mx)*((float)(ix+i2)-mx) 
							      +((float)(jy+j2)-my)*((float)(jy+j2)-my);
								d=(float) Math.min(1,Math.sqrt(d));
								chargeSum0+=chargeMap0[ix+i2][jy+j2]*(1-d);
								countD+=(1-d);
							}
							else test=false;
						}
					}
				
					if (countD>0 && test	){
						chargeSum0=chargeSum0/countD;
						chargeMap1[i][j]=(float) Math.min(1,chargeSum0);
					}
					else chargeMap1[i][j]=0;
				}

			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j]=Math.min(1,chargeMap1[i][j]);
				chargeMap1[i][j]=0;	
			}
		}
		
		////////////////////////////////////////////////////////////////////////
		// detection of hight probability area
		////////////////////////////////////////////////////////////////////////
		for (int i=1;i<mapSize-1;i++){
			for (int j=1;j<mapSize-1;j++){
				if ( chargeMap0[i][j]>0.2
					&& chargeMap0[i][j] >= chargeMap0[i-1][j]
				    && chargeMap0[i][j] >= chargeMap0[i+1][j]
				    && chargeMap0[i][j] >= chargeMap0[i][j-1]
				    && chargeMap0[i][j] >= chargeMap0[i][j+1]  
				                                         
				    && chargeMap0[i][j] >= chargeMap0[i-1][j-1]
				    && chargeMap0[i][j] >= chargeMap0[i+1][j-1]
				    && chargeMap0[i][j] >= chargeMap0[i-1][j+1]
				    && chargeMap0[i][j] >= chargeMap0[i+1][j+1]){
					
					
					chargeMap1[i][j]=1;
					float val=chargeMap0[i][j]*3/4;
					
					for (int i2=-10;i2<=10;i2++){
						for (int j2=-10;j2<=10;j2++){
							if (i+i2>=0 && i+i2<mapSize && j+j2>=0 && j+j2<mapSize){
								if (chargeMap0[i+i2][j+j2]>val) chargeMap1[i+i2][j+j2]=1;
							}
						}
					}
				}
			}
		}
		
	}
	
	
}