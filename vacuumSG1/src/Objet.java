import java.awt.Color;
import java.util.ArrayList;


public class Objet {

	public Color color;
	public Float value;
	public ArrayList<Float>   matLinks;
	public ArrayList<Integer> matIndex;
	
	public Objet(Color c){
		color=c;
		value=null;
		matLinks=new ArrayList<Float>();
		matIndex=new ArrayList<Integer>();
	}
	
	public void setValue(float val){
		if (value==null) value=val;
		else             value= (value+val*5)/6;
	}
	
	public void setLink(int index,float coef){
		int i=matIndex.indexOf(index);
		if (i==-1){
			matLinks.add(coef);
			matIndex.add(index);
		}
		else matLinks.set(i,coef);
		
	}
	
	public boolean isConnected(){
		return (!matIndex.isEmpty());
	}
	
	public void removeLink(int index){
		int index2=matIndex.indexOf(index);
		if (index2>=0){
			matLinks.remove(index2);
			matIndex.remove(index2);
		}
	}
	
	public void correctLink(int index,float newLink){
		int index2=matIndex.indexOf(index);
		if (index2>=0) matLinks.set(index2, newLink);
	}
}
