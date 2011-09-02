import java.awt.Color;
import java.util.ArrayList;


public class InternalMap {

	public float[] map;
	//public ArrayList<Color> objList;
	//public ArrayList<Float> valuesList;
	public ObjectMemory objMemory;
	
	public int imax;
	public float max;
	
	public InternalMap(ObjectMemory m){
		map = new float[180];
		for (int i=0;i<180;i++){
			map[i]=0;
		}
		//objList=new ArrayList<Color>();
		//valuesList=new ArrayList<Float>();
		objMemory=m;
		imax=90;
	}
	
	/*
	public int addObj(Color c,float v){
		int index=objList.indexOf(c);
		if (index==-1){
			objList.add(c);
			valuesList.add(v);
		}
		else{
			valuesList.set(index, (v+valuesList.get(index)*5)/6 );
		}
		return (objList.size());
	}*/
	
	public int compute(double[] d,Color[] c){
		float[] tempMap=new float[180];
		float[] attract1=new float[180];
		float[] attract2=new float[180];
		int index=-1;
		
		
		for (int i=0;i<180;i++){
			//index=objList.indexOf(c[i]);
			index=objMemory.objectList.indexOf(c[i]);
			
			if (index==-1 || objMemory.value.size()<=index || objMemory.value.get(index)==null) tempMap[i]=0;
			else                                              {
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
