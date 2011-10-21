import java.awt.Color;
import java.util.ArrayList;




public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	public float[][] worldMap;
	public ArrayList<Float> TranslationX;
	public ArrayList<Float> TranslationY;
	public ArrayList<Float> Rotation;
	
	public Colliculus(TactileMap tact,VisualMap v){
		tmap=tact;
		vmap=v;

		TranslationX=new ArrayList<Float>();
		TranslationY=new ArrayList<Float>();
		Rotation    =new ArrayList<Float>();

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

}