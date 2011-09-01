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
			//value.add(null);
			
			// compute value as weighted sum of color distances
			float val=0;
			float count=1;
			for (int i=0;i<objectList.size()-1;i++){
				double coef= 1 / ( Math.sqrt(  (rgb.getRed()  -objectList.get(i).getRed())  *(rgb.getRed()  -objectList.get(i).getRed())
		                  				      +(rgb.getGreen()-objectList.get(i).getGreen())*(rgb.getGreen()-objectList.get(i).getGreen())
		                 				      +(rgb.getBlue() -objectList.get(i).getBlue()) *(rgb.getRed()  -objectList.get(i).getBlue()) 
		               					    )/10 +1);
				val+= value.get(i)*coef;
				count+=coef;
				
			}
			value.add(val/count);
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
		else                        value.set(index,(value.get(index)*5+val)/6);
	}
	
	public void setValue(Color rgb,float val){
		int index=objectList.indexOf(rgb);
		if (index!=-1){
			if (value.get(index)==null) value.set(index, val);
			else                        value.set(index,(value.get(index)*5+val)/6);
		}
	}
	
	public int indexOfColor(Color rgb){
		return objectList.indexOf(rgb);
	}
	
}
