import java.awt.Color;
import java.util.ArrayList;


/**
 * Visual colliculus
 * @author simon
 */
public class VisualMap {

	public Color[][] colorMap;
	public float[][] potentialMap;
	public float[][] potentialMapOld;
	
	public float chargeMap0[][][];
	public float chargeMap1[][][];
	public float chargeMapP[][][];
	public int maximumMap[][];
	public int output[][];
	private boolean[][] chargeTestMap;
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
		potentialMap=new float[mapSizeTheta][mapSizeR];
		potentialMapOld=new float[mapSizeTheta][mapSizeR];
		chargeMap0=new float[mapSize][mapSize][10];
		chargeMap1=new float[mapSize][mapSize][10];
		chargeMapP=new float[mapSizeTheta][mapSizeR][10];
		maximumMap=new int[mapSizeTheta][mapSizeR];
		output=new int[mapSizeTheta][4];
		
		chargeTestMap=new boolean[mapSize][mapSize];
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
				
				polar2cartesianX[i][j]=((double)j*Math.cos( ((double)(i*2+90))*Math.PI/180))+mapSize/2;
				polar2cartesianY[i][j]=((double)j*Math.sin( ((double)(i*2+90))*Math.PI/180))+mapSize/2;

				polar2cartesian2X[i][j]=( (float)j * Math.cos((float)i*Math.PI/180) );
				polar2cartesian2Y[i][j]=( (float)j * Math.sin((float)i*Math.PI/180) );
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				potentialMap[i][j]=0;
				potentialMapOld[i][j]=0;
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
				testMapP[i][j]=false;
			}
		}
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize;j++){
				for (int k=0;k<10;k++){
					chargeMap0[i][j][k]=chargeMap1[i][j][k];
				}
				testMap[i][j]=false;
				chargeTestMap[i][j]=false;
			}
		}
		
		///////////////////////////////////////////////////////
		// set colors on polar map
		///////////////////////////////////////////////////////
		for (int i=0;i<mapSizeTheta;i++){
			
			//int confidence=Math.min(40, Math.abs(i-90)/2+1);
			int min=(int) r[i]-1;//(int) Math.max(0, r[i]-confidence/2);
			int max=(int) r[i]+5;//(int) Math.min(mapSizeR, r[i]+confidence/2+1);

			for (int j=0;j<mapSizeR;j++){

				if (j>=min && j<max){
					colorMap[i][j]=c[i];
					confidenceMap[i][j]=5;
				}
				else{
					if (j<min){
						colorMap[i][j]=Color.black;
						confidenceMap[i][j]=5;
					}
					else{
						colorMap[i][j]=null;
						confidenceMap[i][j]=-1;
					}
				}
				
			}
			
			for (int j=0;j<Math.min(100,max);j++){
				testMapP[i][j]=true;
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
				
				for (int i2=-2;i2<=2;i2++){
					for (int j2=-2;j2<=2;j2++){
						if (i+i2>=0 && i+i2<mapSizeTheta && j+j2>=0 && j+j2<mapSizeR){
							if (confidenceMap[i+i2][j+j2]>=0 && !colorMap[i+i2][j+j2].equals(Color.black)){
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
				
			}
		}
		
		///////////////////////////////////////////////////////
		// fill charge map (Cartesian)
		///////////////////////////////////////////////////////
		double theta0,r0;
		double sum0[];
		sum0=new double[10];
		double count0,d;
		int x2i2,y2j2;
		for (int i=0;i<mapSize;i++){
			for (int j=0;j<mapSize/2+1;j++){
				
					
				r0=cartesian2polarR[i][j];
				
				if (j-mapSize/2<=0){
					theta0=cartesian2polarT[i][j];;
					
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
					
					if (x2>=0 && x2<mapSizeR && y2>=0 && y2<mapSizeTheta){
						testMap[i][j]=testMapP[y2][x2];
					}
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
		
		
		// compute flow
		float fx,fy;
		int l=1;
		boolean test1,test2;
		int iml,ipl,jml,jpl;
		for (int i=45+l;i<135-l;i++){
			for (int j=l;j<40;j++){
				test1=true;
				test2=true;
				
				iml=i-l;
				ipl=i+l;
				jml=j-l;
				jpl=j+l;
				
				if (( ( potentialMapOld[iml][j]>potentialMapOld[i][j] && potentialMapOld[ipl][j]<potentialMapOld[i][j])
							 ||( potentialMapOld[iml][j]<potentialMapOld[i][j] && potentialMapOld[ipl][j]>potentialMapOld[i][j]) )
 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[ipl][j]>0 && potentialMap[ipl][j]<1 && potentialMapOld[ipl][j]>0 && potentialMapOld[ipl][j]<1
						  && potentialMap[iml][j]>0 && potentialMap[iml][j]<1 && potentialMapOld[iml][j]>0 && potentialMapOld[iml][j]<1
						  
						  && testMapP[i][j] && testMapP[iml][j] && testMapP[ipl][j] && testMapP[i][jml] && testMapP[i][jpl]
						  && speed>1){
					
					fx=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[iml][j]-potentialMapOld[ipl][j]) );
					
				}
				else{
					test1=false;
					fx=0;
				}
				
				if (( ( potentialMapOld[i][jml]>potentialMapOld[i][j] && potentialMapOld[i][jpl]<potentialMapOld[i][j])
							 ||( potentialMapOld[i][jml]<potentialMapOld[i][j] && potentialMapOld[i][jpl]>potentialMapOld[i][j]) )
						
							 
						  && potentialMap[i  ][j]>0 && potentialMap[i  ][j]<1 && potentialMapOld[i  ][j]>0 && potentialMapOld[i  ][j]<1
						  && potentialMap[i][jpl]>0 && potentialMap[i][jpl]<1 && potentialMapOld[i][jpl]>0 && potentialMapOld[i][jpl]<1
						  && potentialMap[i][jml]>0 && potentialMap[i][jml]<1 && potentialMapOld[i][jml]>0 && potentialMapOld[i][jml]<1
						  
						  && testMapP[i][j] && testMapP[iml][j] && testMapP[ipl][j] && testMapP[i][jml] && testMapP[i][jpl]
						  && speed>1){
					
					fy=  (1/speed)* ( (potentialMap[i][j]-potentialMapOld[i][j]) / (potentialMapOld[i][jml]-potentialMapOld[i][jpl]) );
					
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

		////////////////////////////////////////////////////////////////////////
		// reduce noise
		////////////////////////////////////////////////////////////////////////
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
					mx= (float) polar2cartesian2X[i][j];
					my= (float) polar2cartesian2Y[i][j];
					
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
                               
                	imap=i-mapSize/2;
                    jmap=j-mapSize/2;
                	
                                fx= (float)(imap*Math.cos(mRotation.get(act)) )
                                  - (float)(jmap*Math.sin(mRotation.get(act)) );
                                fy= (float)(imap*Math.sin(mRotation.get(act)) ) 
                                  + (float)(jmap*Math.cos(mRotation.get(act)) );
                                
                                fx-=imap;
                                fy-=jmap;
                                
                                flowX3.get(act)[i][j]=fx +mTranslationX.get(act);
                                flowY3.get(act)[i][j]=fy +mTranslationY.get(act);
                }
        }
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
	
	/*
	public void seeEnvironment2(double[] r,Color[] c,int[] corners, int act,float speed){
		
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
		
	}*/
	
	
}