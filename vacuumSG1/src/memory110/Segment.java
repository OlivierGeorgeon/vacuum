package memory110;

import java.awt.Color;

import javax.vecmath.Vector3f;

public class Segment {

	public Point firstPoint;
	public Point secondPoint;
	
	public Color color;
	
	public boolean complete;
	
	public Vector3f position;
	public Vector3f speed;
	
	public Segment(Point first,Point second){
		firstPoint=first;
		secondPoint=second;
		
		color=first.rightColor;
		complete= firstPoint.type==0 && secondPoint.type==0;
		
		position=new Vector3f(0,0,0);
		position.x=(first.position.x+second.position.x)/2;
		position.y=(first.position.y+second.position.y)/2;
		
		speed=new Vector3f(0,0,0);
		speed.x=(first.speed.x+second.speed.x)/2;
		speed.y=(first.speed.y+second.speed.y)/2;
		
		
	}
}
