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
	
	public void addObject(Color rgb){
		int index = objectList.indexOf(rgb);
		if (index==-1){
			objectList.add(rgb);
			value.add(null);
		}
	}
	
	public void loadObject(Color rgb,float val){
		int index = objectList.indexOf(rgb);
		if (index==-1){
			objectList.add(rgb);
			value.add(val);
		}
	}
	
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
	
	public int indexOfColor(Color rgb){
		return objectList.indexOf(rgb);
	}
	
	/*
	public void linkNewMatrix(int index){
		for (int i=0;i<objectList.size();i++){
			if (objectList.get(i).isConnected()) objectList.get(i).setLink(index, icoef);
			else                                 objectList.get(i).setLink(index, 1); //object without matrix
		}
	}*/
	
}
