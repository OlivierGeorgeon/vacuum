package memory110;

import java.awt.Color;

import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;

import ernest.Ernest;

public class Point {

	public Vector3f position=new Vector3f(0,0,0);
	public Vector3f positionAllocentric=new Vector3f(0,0,0);
	public int angle;
	public double distance;
	
	public int ix,jy;
	
	public Color leftColor=Color.black;
	public Color rightColor=Color.black;
	
	public int type;
	
	public Vector3f speed=new Vector3f();
	
	public float m_relativeOrientation = Ernest.INFINITE;
	
	public Point(float x, float y, int a, int t){
		angle=a;
		type=t;
		position=new Vector3f(x,y,0);
		positionAllocentric=new Vector3f(x,y,0);
		distance=Math.sqrt(x*x+y*y);
		
		speed=new Vector3f();
	}

	
	/**
	 * create point according to the distance
	 * @param d  distance from the agent
	 * @param i  angle of the point (in degree)
	 * @param t
	 */
	public Point(double d, int i,int t){
		angle=i;
		type=t;
		distance=d;
		/*double a=(double) (-i+90)*Math.PI/180;
		/*position=new Vector3f((float)(d*Math.cos(a)),
				              (float)(d*Math.sin(a)),
				              0);*/
		speed=new Vector3f();
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
	
	/**
	 * change the reference orientation
	 */
	public void rotate(double rad, int deg){
		angle=(angle+deg+360)%360;
		
		double x,y;
		x=(float) (position.x*Math.cos(rad) -position.y*Math.sin(rad));
		y=(float) (position.x*Math.sin(rad) +position.y*Math.cos(rad));
		
		position.x=(float) x;
		position.y=(float) y;
		
		x=(float) (speed.x*Math.cos(rad) -speed.y*Math.sin(rad));
		y=(float) (speed.x*Math.sin(rad) +speed.y*Math.cos(rad));
		
		speed.x=(float) x;
		speed.y=(float) y;
	}
	
	public void setSpeed(Vector3f s){
		speed=s;
	}
	
	public void addSpeed(Vector3f s){
		speed.add(s);
	}
	
	public void subSpeed(Vector3f s){
		speed.sub(s);
	}
	
	public void addRotation(Vector3f r){
		Vector3f localSpeed=new Vector3f();
		
		localSpeed.scale(0);
        localSpeed.y=(float) distance * r.z;
        
        Matrix3f rot = new Matrix3f();
        rot.rotZ((float) ( -(angle*Math.PI/180)));
        rot.transform(localSpeed, localSpeed);

        speed.add(localSpeed);
	}
	
	public float getAngle()
	{
		return (float)Math.atan2((double)position.y, position.x);			
	}
}
