import java.awt.Color;
import java.util.ArrayList;

/**
 * compute the most attractive point on the retina
 */
public class InternalMap {

	public float[] map;
	public ObjectMemory objMemory;
	
	/** The angle of the salience */
	public int imax;
	public float max;
	
	public InternalMap(ObjectMemory m){
		map = new float[180];
		for (int i=0;i<180;i++){
			map[i]=0;
		}
		objMemory=m;
		imax=90;
	}
	
	
	/**
	 * Computes the angle of the most attractive point
	 * @param d Vector of distances
	 * @param c Vector of colors
	 * @return The angle of the object that has the highest value. 
	 */
	public int compute(double[] d,Color[] c){
		float[] tempMap=new float[180];
		float[] attract1=new float[180];
		float[] attract2=new float[180];
		int index=-1;
		
		
		for (int i=0;i<180;i++){
			index=objMemory.objectList.indexOf(c[i]);
			
			if (index==-1 || objMemory.value.size()<=index || objMemory.value.get(index)==null){
				tempMap[i]=0;
			}
			else{
				tempMap[i]=(float) (objMemory.value.get(index)/ Math.max(1,d[i]));
			}
		}
		attract1[0]  =tempMap[0];
		attract2[179]=tempMap[179];

		
		for (int i=1;i<179;i++){
			attract1[i]= attract1[i-1] + (tempMap[i]-attract1[i-1])/10;
			attract2[179-i]= attract2[180-i] + (tempMap[179-i]-attract2[180-i])/10;
		}	
		
		max=Math.min(attract1[90],attract2[90]);;
		imax=90;
		for (int i=90;i<180;i++){
			map[i]=Math.min(attract1[i],attract2[i]);
			if (map[i]>max){
				max=map[i];
				imax=i;
			}
		}
		for (int i=89;i>=0;i--){
			map[i]=Math.min(attract1[i],attract2[i]);
			if (map[i]>max){
				max=map[i];
				imax=i;
			}
		}
		return imax;
	}

}
