import java.awt.Color;
import java.util.ArrayList;

import ernest.Ernest;

import spas.IPlace;
import spas.Place;



/**
 * a global colliculus that return salience points of the immediate environment.
 * @author simon
 */
public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	public ArrayList<Float> TranslationX;
	public ArrayList<Float> TranslationY;
	public ArrayList<Float> Rotation;

	
	public int[][] bundles;
	public Color[][] bundleColor;
	
	public float[][][] visualProba;
	public float[][][] tactileProba;
	
	public float bundleMapProba[][];
	public int   bundleMapType[][][];
	
	//public ArrayList<ISalience> liste;

	public Colliculus(TactileMap tact,VisualMap v){
		tmap=tact;
		vmap=v;

		TranslationX=new ArrayList<Float>();
		TranslationY=new ArrayList<Float>();
		Rotation    =new ArrayList<Float>();

		visualProba=new float[100][100][11];
		tactileProba=new float[100][100][5];
		bundleMapProba=new float[100][100];
		bundleMapType =new int[100][100][2];
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				bundleMapProba[i][j]=0;
				bundleMapType[i][j][0]=0;
				bundleMapType[i][j][1]=0;
				
				for (int k=0;k<11;k++){
					visualProba[i][j][k]=0;
				}
				for (int k=0;k<5;k++){
					tactileProba[i][j][k]=0;
				}
			}	
		}
		
		bundles=new int[11][5];
		bundleColor=new Color[12][6];
		for (int i=0;i<11;i++){
			for (int j=0;j<3;j++){
				bundles[i][j]=0;
			}	
		}
		bundleColor[0][0]=Color.black;
		
		bundleColor[0][1]=new Color(240,240,240);
		bundleColor[0][2]=new Color(70,70,70);
		bundleColor[0][3]=new Color(170,170,170);
		bundleColor[0][4]=new Color(130,130,130);
		bundleColor[0][5]=new Color(100,50,50);
		
		bundleColor[1][0]=new Color(220,220,250);
		bundleColor[2][0]=new Color(0,128,0);
		bundleColor[3][0]=new Color(115,230,0);
		bundleColor[4][0]=new Color(150,128,255);
		bundleColor[5][0]=new Color(46,230,0);
		bundleColor[6][0]=new Color(0,230,230);
		bundleColor[7][0]=new Color(0,230,92);
		bundleColor[8][0]=new Color(230,207,0);
		bundleColor[9][0]=new Color(0,230,161);
		bundleColor[10][0]=new Color(184,230,0);
		bundleColor[11][0]=new Color(100,100,100);
		
		for (int i=1;i<12;i++){
			for (int j=1;j<6;j++){
				bundleColor[i][j]= new Color( (bundleColor[i][0].getRed()+bundleColor[0][j].getRed() )/2,
											  (bundleColor[i][0].getGreen()+bundleColor[0][j].getGreen() )/2,
											  (bundleColor[i][0].getBlue()+bundleColor[0][j].getBlue() )/2);
			}
		}

			
         TranslationX.add(0f);
         TranslationX.add(0f);
         TranslationX.add(0f);

         TranslationY.add(-0.333f);
         TranslationY.add(0f);
         TranslationY.add(0f);

         Rotation.add(0f);
         Rotation.add(0.1f);
         Rotation.add(-0.1f);
		 
	}
	
	/**
	 * set environment configuration in polar referential to be "read" by sensors
	 * of partial colliculus.
	 * the parameters r, c, rm and cm are given by the rendu function of Ernest100Model and
	 * give information given by the 1 dimension retina
	 * @param r       Vector that give the distance of objects around the agent for each theta
	 * @param c       Colors of objects around the agent 
	 * @param rm      Vector that give distances in front of the agent
	 * @param cm      Colors of objects in front of the agent
	 * @param action  Current action performed by the agent
	 * @param speed   Value of the action
	 */
	public void update(double[] rv,Color[] c,double[] rt,int[] t,int action,float speed){
		
		// add new coefficient set
		if (Rotation.size()<action+1){
			while (Rotation.size()<action+1){
				TranslationX.add((float) 0);
				TranslationY.add((float) 0);
				Rotation.add((float) 0);
			}
		}
		
		vmap.seeEnvironment(rv, c);
		vmap.coefficients(action, speed);
		//TranslationX=vmap.mTranslationX;
		//TranslationY=vmap.mTranslationY;
		//Rotation    =vmap.mRotation;
		vmap.moveCharges(TranslationX.get(action), TranslationY.get(action), Rotation.get(action), speed);
		
		tmap.touchEnvironment(rt, t);
		tmap.coefficients(action, speed);
		tmap.moveCharges(-TranslationX.get(action), -TranslationY.get(action), -Rotation.get(action), speed);
/*
		computeBundleMap();
		
		// generate bundles
		// for each cell
		for (int i=1;i<99;i++){
			for (int j=1;j<99;j++){
				// for each tactile stimuli
				for (int i2=0;i2<5;i2++){
					if (tmap.chargeMap1[i][j][i2]>0.99
							&& tmap.chargeMap1[i+1][j][i2]>0.8
							&& tmap.chargeMap1[i-1][j][i2]>0.8
							&& tmap.chargeMap1[i][j+1][i2]>0.8
							&& tmap.chargeMap1[i][j-1][i2]>0.8){
						
						// for each visual stimuli
						for (int j2=0;j2<11;j2++){
							if (vmap.chargeMap1[i][j][j2]>0.99
									&& vmap.chargeMap1[i+1][j][j2]>0.8
									&& vmap.chargeMap1[i-1][j][j2]>0.8
									&& vmap.chargeMap1[i][j+1][j2]>0.8
									&& vmap.chargeMap1[i][j-1][j2]>0.8){
								bundles[j2][i2]=1;
							}
						}
						
					}
				}
			}
		}
		*/
	}
	
	/**
	 * get the list of salience points around the agent
	 * @return the list of salience points
	 */
	public  ArrayList<IPlace> getPlaceList(){
		ArrayList<IPlace> list;
		list=new  ArrayList<IPlace>();

		int theta=0,thetaMin=0,thetaMax=0;
		float span;
		float dSum=0;
		int count=0;
		float d=0;
		int val=0;
		
		boolean salience=false;
		boolean stop=false;
		
		// Tactile salience points
		int j=0;
		int i=0;
		int mapSizeTheta=tmap.mapSizeTheta;
		while ( (j<mapSizeTheta || salience) && j<2*mapSizeTheta ){
			
			i=j%mapSizeTheta;
			
			// if new salience point detected
			if (!salience && tmap.output[i][0]>0){
				salience=true;
				thetaMin=j;
				val=tmap.output[i][0];
			}
			
			// close salience point
			if (salience && (tmap.output[i][0]!=val || stop)){
				salience=false;
				stop=false;
				thetaMax=j-1;
				
				theta=(thetaMax+thetaMin)/2;
				span=thetaMax-thetaMin;
				d=dSum/(float)count;
				
				dSum=0;
				count=0;
				// create Salience
				if (thetaMin>0){
					if      (val==0) val=Ernest.STIMULATION_TOUCH_EMPTY;
					else if (val==1) val=Ernest.STIMULATION_TOUCH_WALL;
					else if (val==2) val=Ernest.STIMULATION_TOUCH_SOFT;
					else if (val==3) val=Ernest.STIMULATION_TOUCH_FISH;
					else if (val==4) val=Ernest.STIMULATION_GUSTATORY_CUDDLE;
					//list.add(new Salience(val, Ernest.MODALITY_TACTILE, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
					// TODO pass a bundle
					list.add(new Place(null, d, (float)((theta-90)*Math.PI/180), (float)(span*Math.PI/180) ) );
				}
			}
			
			// scan a salience point
			if (salience){
				dSum+=(float)(tmap.output[i][3]+tmap.output[i][2])/2;
				count++;
				
				int next=(j+1)%mapSizeTheta;

				if (  tmap.output[next][2] > tmap.output[i][3] || tmap.output[next][3] < tmap.output[i][2] ){
					stop=true;
				}
			}
			j++;
		}
		
		
		
		// Visual salience points
		
		salience=false;
		stop=false;
		
		dSum=0;
		count=0;

		j=0;
		i=0;
		mapSizeTheta=vmap.mapSizeTheta;
		while ( (j<mapSizeTheta || salience) && j<2*mapSizeTheta ){
			
			i=j%mapSizeTheta;
			
			// if new salience point detected
			if (!salience && vmap.output[i][0]>0){
				salience=true;
				thetaMin=j;
				val=vmap.output[i][0];
			}
			
			// close salience point
			if (salience && (vmap.output[i][0]!=val || stop)){
				salience=false;
				stop=false;
				thetaMax=j-1;
				
				theta=(thetaMax+thetaMin)/2;
				span=thetaMax-thetaMin;
				d=dSum/count;
				dSum=0;
				count=0;
				// create Salience
				if (thetaMin>0){
					if      (val==1) val=32768;       //   0*65536 + 128*256 +   0
					else if (val==2) val=7595520;     // 115*65536 + 230*256 +   0
					else if (val==3) val=9863423;     // 150*65536 + 128*256 + 255
					else if (val==4) val=3073536;     //  46*65536 + 230*256 +   0
					else if (val==5) val=59110;       //   0*65536 + 230*256 + 230
					else if (val==6) val=58972;       //   0*65536 + 230*256 +  92
					else if (val==7) val=15126272;    // 230*65536 + 207*256 +   0
					else if (val==8) val=59041;       //   0*65536 + 230*256 + 161
					else if (val==9) val=12117504;    // 184*65536 + 230*256 +   0
					else if (val==10)val=6579300;     // 100*65536 + 100*256 + 100
					// list.add(new Salience(val, Ernest.MODALITY_VISUAL, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
					// TODO pass a bundle
					list.add(new Place(null, d, (float)((theta-90)*Math.PI/180), (float)(span*Math.PI/180) ) );
				}
			}
			
			// scan a salience point
			if (salience){
				dSum+=(float)(vmap.output[i][3]+vmap.output[i][2])/2;
				count++;
				int next=(j+1)%mapSizeTheta;
				
				if (  vmap.output[next][2] > vmap.output[i][3] || vmap.output[next][3] < vmap.output[i][2] ){
					stop=true;
				}
			}
			j++;
		}

		//liste=(ArrayList<ISalience>) list.clone();

		return list;
	}
	
	private void computeBundleMap(){
		float p1=0;
		float p2=0;
		float Sp1=0;
		float Sp2=0;
		
		float max=0;
		
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				
				bundleMapProba[i][j]=0;
				max=0;
				for (int i2=0;i2<11;i2++){
					Sp1+=vmap.chargeMap1[i][j][i2];
				}
				for (int j2=0;j2<5;j2++){
					Sp2+=tmap.chargeMap1[i][j][j2];
				}
				
				Sp1=(1 - Math.min(1,Sp1) )/11;
				Sp2=(1 - Math.min(1,Sp2) )/5;
				
				
				for (int i2=0;i2<11;i2++){
					visualProba[i][j][i2]=vmap.chargeMap1[i][j][i2]+ Sp1;
				}
				for (int j2=0;j2<5;j2++){
					tactileProba[i][j][j2]=tmap.chargeMap1[i][j][j2]+ Sp2;
				}
				
				for (int i2=0;i2<11;i2++){
					for (int j2=0;j2<5;j2++){
						if (bundles[i2][j2]!=0){
							p1=vmap.chargeMap1[i][j][i2]+ Sp1;
							p2=tmap.chargeMap1[i][j][j2]+ Sp2;

							if (p1*p2>max){
								max=p1*p2;
								bundleMapProba[i][j]=max;
								bundleMapType[i][j][0]=i2;
								bundleMapType[i][j][1]=j2;
							}
						}
					}
				}
				
			}
		}
		
	}

}