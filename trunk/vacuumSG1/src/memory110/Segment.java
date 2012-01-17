package memory110;

import java.awt.Color;

public class Segment {

	public Point firstPoint;
	public Point secondPoint;
	
	public Color color;
	
	public boolean complete;
	
	public Segment(Point first,Point second){
		firstPoint=first;
		secondPoint=second;
		
		color=first.rightColor;
		complete= firstPoint.type==0 && secondPoint.type==0;
	}
}
