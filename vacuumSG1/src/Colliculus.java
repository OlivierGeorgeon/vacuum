import java.awt.Color;




public class Colliculus {
	
	public TactileMap tmap;
	double[] retine;
	Color[] colorMap;
	
	public Colliculus(TactileMap t){
		tmap=t;
		retine=new double[180];
		colorMap=new Color[180];
	}
	
	public void updateRetine(double[] r,Color[] cm){
    	retine=r;
    	colorMap=cm;
    }
	
}
