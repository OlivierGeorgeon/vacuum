import java.awt.Color;
import java.util.ArrayList;


public class ObjectMemory {

	public ArrayList<Color> objectList;
	public  ArrayList<Float> value;
	
	float icoef=(float) 0.3;
	
	public ObjectMemory(){
		objectList=new ArrayList<Color>();
		value =new ArrayList<Float>();
	}
	
	// add a new object
	public void addObject(Color rgb){
		int index = objectList.indexOf(rgb);
		if (index==-1){
			objectList.add(rgb);
			value.add(null);
		}
	}
	
	// add an object whose value is known
	public void loadObject(Color rgb,float val){
		int index = objectList.indexOf(rgb);
		if (index==-1){
			objectList.add(rgb);
			value.add(val);
		}
	}
	
	// set a value to an object.
	// if the value was already determined, a weighted value is set
	public void setValue(int index,float val){
		if (value.get(index)==null) value.set(index, val);
		else                        value.set(index,(value.get(index)+val*5)/6);
	}
	
	public void setValue(Color rgb,float val){
		int index=objectList.indexOf(rgb);
		if (index!=-1){
			if (value.get(index)==null) value.set(index, val);
			else                        value.set(index,(value.get(index)+val*5)/6);
		}
	}
	
	// return the index of the object of color rgb
	public int indexOfColor(Color rgb){
		return objectList.indexOf(rgb);
	}
	
	
}
