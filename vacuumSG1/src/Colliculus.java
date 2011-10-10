import java.awt.Color;
import java.util.ArrayList;




public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	
	public Colliculus(TactileMap t,VisualMap v){
		tmap=t;
		vmap=v;
	}
	
	public void updateTactile(double[] r,Color[] c,int action,float speed){
		tmap.touchEnvironment(r, c, action, speed);
	}
	
	public void updateRetine(double[] r,Color[] cm,int action,float speed){
		vmap.seeEnvironment(r, cm, action, speed);
    }
	
}