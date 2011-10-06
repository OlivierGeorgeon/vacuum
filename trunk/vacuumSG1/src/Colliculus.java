import java.awt.Color;
import java.util.ArrayList;




public class Colliculus {
	
	public TactileMap tmap;
	public VisualMap vmap;
	public ArrayList<int[]> bundleList;    // list of tactile-color bundle
	
	public Colliculus(TactileMap t,ErnestModel e){
		tmap=t;
		vmap=new VisualMap(e);
	}
	
	public void updateTactile(double[] r,Color[] c,int action,float speed){
		tmap.touchEnvironment(r, c, action, speed);
	}
	
	public void updateRetine(double[] r,Color[] cm){
		vmap.seeEnvironement(r, cm);
    }
	
}