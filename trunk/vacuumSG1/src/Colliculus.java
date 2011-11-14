import java.awt.Color;
import java.util.ArrayList;

import ernest.Ernest;

import spas.ISalience;
import spas.Salience;



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
	public int[][] tactileStimulis;
	public int[][] visualStimulis;
	
	public int[][] bundles;
	
	//public ArrayList<ISalience> liste;

	public Colliculus(TactileMap tact,VisualMap v){
		tmap=tact;
		vmap=v;

		TranslationX=new ArrayList<Float>();
		TranslationY=new ArrayList<Float>();
		Rotation    =new ArrayList<Float>();
		
		tactileStimulis=new int[180][100];
		visualStimulis =new int[180][100];
		
		
		bundles=new int[10][4];
		for (int i=0;i<10;i++){
			for (int j=0;j<3;j++){
				bundles[i][j]=0;
			}	
		}
		
         TranslationX.add(0f);
         TranslationX.add(0f);
         TranslationX.add(0f);

         TranslationY.add(-0.333f);
         TranslationY.add(0f);
         TranslationY.add(0f);

         Rotation.add(0f);
         Rotation.add(-0.00175f);
         Rotation.add(0.00175f);
		 
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
	public void update(double[] r,Color[] c,double[] rm,Color[] cm,int action,float speed){
		
		// add new coefficient set
		if (Rotation.size()<action+1){
			while (Rotation.size()<action+1){
				TranslationX.add((float) 0);
				TranslationY.add((float) 0);
				Rotation.add((float) 0);
			}
		}
		
		vmap.seeEnvironment(rm, cm);
		vmap.coefficients(action, speed);
		//TranslationX=vmap.mTranslationX;
		//TranslationY=vmap.mTranslationY;
		//Rotation    =vmap.mRotation;
		vmap.moveCharges(TranslationX.get(action), TranslationY.get(action), Rotation.get(action), speed);
		
		tmap.touchEnvironment(r, c);
		//tmap.coefficients(action, speed);
		tmap.moveCharges(-TranslationX.get(action), -TranslationY.get(action), -Rotation.get(action), speed);

		
		// generate bundles
		// for each cell
		for (int i=1;i<99;i++){
			for (int j=1;j<99;j++){
				// for each tactile stimuli
				for (int i2=0;i2<4;i2++){
					if (tmap.chargeMap1[i][j][i2]>0.99
							&& tmap.chargeMap1[i+1][j][i2]>0.8
							&& tmap.chargeMap1[i-1][j][i2]>0.8
							&& tmap.chargeMap1[i][j+1][i2]>0.8
							&& tmap.chargeMap1[i][j-1][i2]>0.8){
						
						// for each visual stimuli
						for (int j2=0;j2<10;j2++){
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
		
	}
	
	/**
	 * get the list of salience points around the agent
	 * @return the list of salience points
	 */
	public  ArrayList<ISalience> getSalienceList(){
		ArrayList<ISalience> list;
		list=new  ArrayList<ISalience>();

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
					if      (val==0) val=Ernest.STIMULATION_TOUCH_EMPTY.getValue();
					else if (val==1) val=Ernest.STIMULATION_TOUCH_WALL.getValue();
					else if (val==2) val=Ernest.STIMULATION_TOUCH_SOFT.getValue();
					else if (val==3) val=Ernest.STIMULATION_TOUCH_FISH.getValue();
					list.add(new Salience(val, Ernest.MODALITY_TACTILE, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
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
					list.add(new Salience(val, Ernest.MODALITY_VISUAL, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
				
					
					
					
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

}