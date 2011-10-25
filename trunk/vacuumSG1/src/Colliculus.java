import java.awt.Color;
import java.util.ArrayList;

import spas.ISalience;
import spas.Salience;




public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	public ArrayList<Float> TranslationX;
	public ArrayList<Float> TranslationY;
	public ArrayList<Float> Rotation;
	public int[][] tactileStimulis;
	public int[][] visualStimulis;
	
	//public ArrayList<ISalience> liste;

	public Colliculus(TactileMap tact,VisualMap v){
		tmap=tact;
		vmap=v;

		TranslationX=new ArrayList<Float>();
		TranslationY=new ArrayList<Float>();
		Rotation    =new ArrayList<Float>();
		
		tactileStimulis=new int[180][100];
		visualStimulis =new int[180][100];

	}
	
	public void update(double[] r,Color[] c,double[] rm,Color[] cm,int[] corners,int action,float speed,double[] r3){
		
		// add new coefficient set
		if (Rotation.size()<action+1){
			while (Rotation.size()<action+1){
				TranslationX.add((float) 0);
				TranslationY.add((float) 0);
				Rotation.add((float) 0);
			}
		}
		
		vmap.seeEnvironment(rm, cm, action, speed);
		vmap.coefficients(rm, cm, action, speed);
		TranslationX=vmap.mTranslationX;
		TranslationY=vmap.mTranslationY;
		Rotation    =vmap.mRotation;
		vmap.moveCharges(TranslationX.get(action), TranslationY.get(action), Rotation.get(action), speed);
		
		tmap.touchEnvironment(r, c, action, speed);
		//tmap.coefficients(r, c, action, speed);
		tmap.moveCharges(-TranslationX.get(action), -TranslationY.get(action), -Rotation.get(action), speed);

		
	}
	
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
		
		// tactile salience points
		int j=0;
		int i=0;
		while ( j<tmap.mapSizeTheta || salience ){
			
			i=j%tmap.mapSizeTheta;
			
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
					if      (val==1) val=32768;       //   0*65536 + 128*256 +   0
					else if (val==2) val=7595520;     // 115*65536 + 230*256 +   0
					else if (val==3) val=9863423;     // 150*65536 + 128*256 + 255
					else if (val==4) val=3073536;     //  46*65536 + 230*256 +   0
					else if (val==5) val=59110;       //   0*65536 + 230*256 + 230
					else if (val==6) val=58972;       //   0*65536 + 230*256 +  92
					else if (val==7) val=15126272;    // 230*65536 + 207*256 +   0
					else if (val==8) val=59041;       //   0*65536 + 230*256 + 161
					else if (val==9) val=12117504;    // 184*65536 + 230*256 +   0
					list.add(new Salience(val, 0, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
				}
			}
			
			// scan a salience point
			if (salience){
				dSum+=(float)(tmap.output[i][3]+tmap.output[i][2])/2;
				count++;
				
				int next=(j+1)%tmap.mapSizeTheta;
				
				if (  tmap.output[next][2] > tmap.output[i][3] || tmap.output[next][3] < tmap.output[i][2] ){
					stop=true;
				}
			}
			j++;
		}
		
		
		
		// visual salience points
		
		salience=false;
		stop=false;
		
		dSum=0;
		count=0;

		j=0;
		i=0;
		while ( j<vmap.mapSizeTheta || salience ){
			
			i=j%vmap.mapSizeTheta;
			
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
					list.add(new Salience(val, 1, (float)((theta-90)*Math.PI/180), d, (float)(span*Math.PI/180) ) );
				}
			}
			
			// scan a salience point
			if (salience){
				dSum+=(float)(vmap.output[i][3]+vmap.output[i][2])/2;
				count++;
				int next=(j+1)%vmap.mapSizeTheta;
				
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