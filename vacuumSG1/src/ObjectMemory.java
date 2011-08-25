import java.awt.Color;
import java.util.ArrayList;


public class ObjectMemory {

	public ArrayList<Objet> objectList;
	float icoef=(float) 0.3;
	
	public ObjectMemory(){
		objectList=new ArrayList<Objet>();
	}
	
	public void addObject(Color rgb,float val,ArrayList<Integer> indexMat){
		int index=objectList.size();
		objectList.add(new Objet(rgb));
		objectList.get(index).setValue(val);
		
		for (int i=0;i<indexMat.size();i++){
			objectList.get(index).setLink(indexMat.get(i), icoef);
		}	
	}
	
	public void linkNewMatrix(int index){
		for (int i=0;i<objectList.size();i++){
			if (objectList.get(i).isConnected()) objectList.get(i).setLink(index, icoef);
			else                                 objectList.get(i).setLink(index, 1); //object without matrix
		}
	}
	
}
