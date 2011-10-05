import java.awt.Color;
import java.util.ArrayList;

/**
 * Ernest's memory of objects. 
 * Associates each color with a value.
 */
public class ObjectMemory {

	/** The list of objects */
	public ArrayList<Color> objectList;

	/** The list of values */
	public  ArrayList<Float> value;
	
	float icoef=(float) 0.3;
	
	/** 
	 * Initialize the objectList and the value list. 
	 */
	public ObjectMemory(){
		objectList=new ArrayList<Color>();
		value =new ArrayList<Float>();
	}
	
	/** 
	 * Add a new object in memory, if it does not yet exist.
	 * The value is initialized 
	 * @param rgb The object's color.
	 */
	public void addObject(Color rgb)
	{
		int index = objectList.indexOf(rgb);
		if (index==-1)
		{
			objectList.add(rgb);
			//value.add(null);
			
			// compute value as weighted sum of color distances
			float val=0;
			float count=1;
			for (int i=0;i<objectList.size()-1;i++)
			{
				double coef= 1 / ( Math.sqrt(  (rgb.getRed()  -objectList.get(i).getRed())  *(rgb.getRed()  -objectList.get(i).getRed())
		                  				      +(rgb.getGreen()-objectList.get(i).getGreen())*(rgb.getGreen()-objectList.get(i).getGreen())
		                 				      +(rgb.getBlue() -objectList.get(i).getBlue()) *(rgb.getBlue()  -objectList.get(i).getBlue()) 
		               					    )/10 +1);
				val+= value.get(i)*coef;
				count+=coef;
			}
			value.add(val/count);
		}
	}
	
	/** 
	 * Add a new object to the list if it does not yet exist.
	 * @param rgb The object's color.
	 * @param val The object's value. 
	 */
	public void loadObject(Color rgb,float val){
		int index = objectList.indexOf(rgb);
		if (index==-1){
			objectList.add(rgb);
			value.add(val);
		}
	}
	
	/** 
	 * Update the value of an object.
	 * new_value = (old_value * 5 + val) /6.  
	 * @param index The object's index in the list.
	 * @param val The object's value. 
	 */
	public void setValue(int index,float val){
		if (value.get(index)==null) value.set(index, val);
		else                        value.set(index,(value.get(index)*5+val)/6);
	}
	
	/** 
	 * Update the value of an object.
	 * new_value = (old_value * 5 + val) /6.  
	 * @param rgb The object's color.
	 * @param val The object's value. 
	 */
	public void setValue(Color rgb,float val){
		int index=objectList.indexOf(rgb);
		if (index!=-1){
			if (value.get(index)==null) value.set(index, val);
			else                        value.set(index,(value.get(index)*5+val)/6);
		}
		
	}
	
	/** 
	 * Find the index of a color in the list
	 * @param rgb The object's color.
	 * @return The index in the list. 
	 */
	public int indexOfColor(Color rgb){
		return objectList.indexOf(rgb);
	}
	
}
