package memory110;

import java.awt.Color;

import javax.vecmath.Vector3f;

public class Point {

	public Vector3f position;
	public int angle;
	public double distance;
	
	public Color leftColor=Color.black;
	public Color rightColor=Color.black;
	
	public int type;
	
	public Point(float x, float y, int i, int t){
		angle=i;
		type=t;
		position=new Vector3f(x,y,0);
		distance=Math.sqrt(x*x+y*y);
	}
	
	public void setColors(Color l,Color r){
		leftColor=l;
		rightColor=r;
	}
	
	public void setColorsLeft(Color l){
		leftColor=l;
	}
	
	public void setColorsRight(Color r){
		rightColor=r;
	}
	
	public void rotate(double rad, int deg){
		angle=(angle+deg+360)%360;
		
		double x,y;
		x=(float) (position.x*Math.cos(rad) -position.y*Math.sin(rad));
		y=(float) (position.x*Math.sin(rad) +position.y*Math.cos(rad));
		
		position.x=(float) x;
		position.y=(float) y;
	}
	
}
