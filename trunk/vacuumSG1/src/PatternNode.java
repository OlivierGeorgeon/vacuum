import java.util.ArrayList;


public class PatternNode {

	public int id;
	public float x,y;

	public ArrayList<Integer> output;
	
	
	public PatternNode(int i,float x1,float y1){
		id=i;
		x=x1;
		y=y1;
		output=new ArrayList<Integer>();
	}
	
	public void setPosition(float x1,float y1){
		x=x1;
		y=y1;
	}
	
	public void addOutput(int id1){
		if (id1!=id) output.add(id1);
	}
	
}
