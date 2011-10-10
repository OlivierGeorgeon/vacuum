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
	public ArrayList<float[][]> flowX1;				// real flow
	public ArrayList<float[][]> flowY1;
	public ArrayList<float[][]> flowX2;				// flow with reduced noise
	public ArrayList<float[][]> flowY2;
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
		potentialMap=new float[mapSize][mapSize];
		potentialMapOld=new float[mapSize][mapSize];
		chargeMap0=new float[mapSize][mapSize];
		chargeMap1=new float[mapSize][mapSize];
		confidenceMap=new float[mapSizeTheta][mapSizeR];
		potentialConfidenceMap=new float[mapSize][mapSize];
		testMap=new boolean[mapSize][mapSize];
		flowX1=new ArrayList<float[][]>();
		flowY1=new ArrayList<float[][]>();
		flowX2=new ArrayList<float[][]>();
		flowY2=new ArrayList<float[][]>();
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
	
	
	public void seeEnvironment(double[] r,Color[] c, int act,float speed){
		
		
		// set colors on polar map
		for (int i=0;i<180;i++){
			
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
		}
		
		
		
		///////////////////////////////////////////////////////
		// compute flow
		///////////////////////////////////////////////////////
		
		// add new flow map
		if (flowX1.size()<act+1){
			while (flowX1.size()<act+1){
				flowX1.add(new float[mapSize][mapSize]);
				flowY1.add(new float[mapSize][mapSize]);
				flowX2.add(new float[mapSize][mapSize]);
				flowY2.add(new float[mapSize][mapSize]);
				confidenceFlow.add(new float[mapSize][mapSize]);
				
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
					}
				}
			}
		}
		
		
		// reset maps
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMapOld[i][j]=potentialMap[i][j];
				//potentialMap[i][j]=0;
				potentialConfidenceMap[i][j]=0;
				testMap[i][j]=false;
			}
		}
		
		// generate Cartesian potential map
		float x,y;
		double sum;
		double counter;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize/2+1;j++){
				x= (float) Math.sqrt( (float)((i-25)*(i-25) + (j-25)*(j-25)) );
				if (x>0 && j-25<=0){
					y=0;
					if (j-25 <0) y=(float)( Math.atan( -((float)i-25)/((float)j-25) )*180/Math.PI);
					else if (j-25==0 && i-25>0) y= 90;
					else if (j-25==0 && i-25<0) y=-90;
					
					int x2=Math.round(x*2);
					int y2=Math.round(y+90);
					
					if (y2>=180) y2=179;
					
					counter=0;
					sum=0;
					for (int i2=-6;i2<=6;i2++){
						for (int j2=-6;j2<=6;j2++){
							if (x2+i2>=0 && x2+i2<mapSizeR && y2+j2>=0 && y2+j2<mapSizeTheta){
								if (confidenceMap[y2+j2][x2+i2]>=0){
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
					
					if (i!=mapSize/2 || j!=mapSize/2){
						testMap[i][j]=true;
						chargeMap1[i][j]=potentialMap[i][j];
					}
				}
			}
		}
		
		
		// compute flow
		float fx,fy;
		int l=1;
		for (int i=l;i<mapSize-l;i++){
			for (int j=l;j<mapSize/2+1;j++){
				
				if (Math.abs(potentialMapOld[i-l][j]-potentialMapOld[i+l][j])>0.005
						&& Math.abs(potentialMapOld[i][j]-potentialMapOld[i+l][j])>0.001
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i-l][j])>0.001
						  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMapOld[i+l][j]<=potentialMapOld[i][j])
							 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMapOld[i+l][j]>=potentialMapOld[i][j]) )
 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i+l][j]>0 && potentialMap[i+l][j]<1 && potentialMapOld[i+l][j]>0 && potentialMapOld[i+l][j]<1
						  && potentialMap[i-l][j]>0 && potentialMap[i-l][j]<1 && potentialMapOld[i-l][j]>0 && potentialMapOld[i-l][j]<1
						  
						  && testMap[i][j] && testMap[i-l][j] && testMap[i+l][j] && testMap[i][j-l] && testMap[i][j+l]
						  && speed>1){
					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]) );
					
				}
				else fx=0;
				
				if (Math.abs(potentialMapOld[i][j-l]-potentialMapOld[i][j+l])>0.005
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j+l])>0.001
						  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j-l])>0.001
						  && ( ( potentialMapOld[i][j-l]>=potentialMapOld[i][j] && potentialMapOld[i][j+l]<=potentialMapOld[i][j])
							 ||( potentialMapOld[i][j-l]<=potentialMapOld[i][j] && potentialMapOld[i][j+l]>=potentialMapOld[i][j]) )
						
							 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i][j+l]>0 && potentialMap[i][j+l]<1 && potentialMapOld[i][j+l]>0 && potentialMapOld[i][j+l]<1
						  && potentialMap[i][j-l]>0 && potentialMap[i][j-l]<1 && potentialMapOld[i][j-l]>0 && potentialMapOld[i][j-l]<1
						  
						  && testMap[i][j] && testMap[i-l][j] && testMap[i+l][j] && testMap[i][j-l] && testMap[i][j+l]
						  && speed>1){
					
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]) );
					
				}
				else fy=0;
				
				if (fx!=0 || fy!=0){
					
					if (fx!=0) flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
					if (fy!=0) flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy ) 
					                      /(confidenceFlow.get(act)[i][j]+1);
				
					if (confidenceFlow.get(act)[i][j]<50000) confidenceFlow.get(act)[i][j]++;
				}
				
				
				/*
				if ( Math.abs(potentialMapOld[i-l][j]-potentialMapOld[i+l][j])>0.005
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i+l][j])>0.001
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i-l][j])>0.001
				  && ( ( potentialMapOld[i-l][j]>=potentialMapOld[i][j] && potentialMapOld[i+l][j]<=potentialMapOld[i][j])
					 ||( potentialMapOld[i-l][j]<=potentialMapOld[i][j] && potentialMapOld[i+l][j]>=potentialMapOld[i][j]) )
					 
				  && Math.abs(potentialMapOld[i][j-l]-potentialMapOld[i][j+l])>0.005
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j+l])>0.001
				  && Math.abs(potentialMapOld[i][j]-potentialMapOld[i][j-l])>0.001
				  && ( ( potentialMapOld[i][j-l]>=potentialMapOld[i][j] && potentialMapOld[i][j+l]<=potentialMapOld[i][j])
					 ||( potentialMapOld[i][j-l]<=potentialMapOld[i][j] && potentialMapOld[i][j+l]>=potentialMapOld[i][j]) )
				
					 
				  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
				  && potentialMap[i+l][j]>0 && potentialMap[i+l][j]<1 && potentialMapOld[i+l][j]>0 && potentialMapOld[i+l][j]<1
				  && potentialMap[i-l][j]>0 && potentialMap[i-l][j]<1 && potentialMapOld[i-l][j]>0 && potentialMapOld[i-l][j]<1
				  && potentialMap[i][j+l]>0 && potentialMap[i][j+l]<1 && potentialMapOld[i][j+l]>0 && potentialMapOld[i][j+l]<1
				  && potentialMap[i][j-l]>0 && potentialMap[i][j-l]<1 && potentialMapOld[i][j-l]>0 && potentialMapOld[i][j-l]<1
				  
				  && testMap[i][j] && testMap[i-l][j] && testMap[i+l][j] && testMap[i][j-l] && testMap[i][j+l]
				  && speed>1){

					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i-l][j]-potentialMapOld[i+l][j]) );
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][j-l]-potentialMapOld[i][j+l]) );	
					
					if (fx!=0 || fy!=0){
					
						flowX1.get(act)[i][j]= ( flowX1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fx ) 
						                      /(confidenceFlow.get(act)[i][j]+1);
						flowY1.get(act)[i][j]= ( flowY1.get(act)[i][j]*confidenceFlow.get(act)[i][j] + fy ) 
						                      /(confidenceFlow.get(act)[i][j]+1);
					
						if (confidenceFlow.get(act)[i][j]<50000) confidenceFlow.get(act)[i][j]++;
					}
					
				}
				else if (!testMap[i][j]){
					flowX1.get(act)[i][j]=0;
					flowY1.get(act)[i][j]=0;
					confidenceFlow.get(act)[i][j]=0;
				}*/
			}
		}


		// reduce noise
		int count;
		float mx,my;
		int a=5;
		int k=act;
		int i3,j3;
		for (int i=a;i<mapSize-a;i++){
			for (int j=a;j<mapSize/2;j++){
				if (testMap[i][j] && flowX1.get(k)[i][j]!=0 && flowY1.get(k)[i][j]!=0){
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
		int count2=0;
		mx=0;
		my=0;
		float mTheta=0;
		double theta0,theta1;
		for (int i=a;i<mapSize-a;i++){
			for (int j=a;j<mapSize-a;j++){
				if (testMap[i][j] && confidenceFlow.get(act)[i][j]>10){
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
				if (!testMap[i][j] || confidenceFlow.get(act)[i][j]==0){
					
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
		
		
////////////////////////////////////////////////////////////////////////
		// move charges
		////////////////////////////////////////////////////////////////////////
		
		mx=my=0;
		float d=0;
		float countD=0;
		float chargeSum0=0;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				if (!testMap[i][j]){
					mx=(float)i-flowX2.get(act)[i][j]*speed;
					my=(float)j-flowY2.get(act)[i][j]*speed;
			
					int ix=Math.round(mx);
					int jy=Math.round(my);
				
					chargeSum0=0;
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (ix+i2>=0 && ix+i2<mapSize && jy+j2>=0 && jy+j2<mapSize){
								d= ((float)(ix+i2)-mx)*((float)(ix+i2)-mx) 
							      +((float)(jy+j2)-my)*((float)(jy+j2)-my);
								d=(float) Math.min(1,Math.sqrt(d));
								chargeSum0+=chargeMap0[ix+i2][jy+j2]*(1-d);
								countD+=(1-d);
							}
						}
					}
				
					if (countD>0){
						chargeSum0=chargeSum0/countD;
						if (!testMap[i][j]){
							chargeMap1[i][j]=(float) Math.min(1,chargeSum0);
						}
						else{
							chargeMap1[i][j]=potentialMap[i][j];
						}
					}
				}

			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				chargeMap0[i][j]=Math.min(1,chargeMap1[i][j]);
				chargeMap0[i][j]=Math.min(1,chargeMap1[i][j]);
				chargeMap1[i][j]=0;
				chargeMap1[i][j]=0;		
			}
		}
		

	}
	
	
}