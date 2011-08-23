

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;

import ernest.*;
import tracing.*;

/**************************************
 * A Model for Ernest 
 * (This model works with Ernest 8.2 in a grid where there is no singularties in walls)
 * (http://e-ernest.blogspot.com/2011/01/ernest-82-can-find-his-food.html)
 * @author ogeorgeon
 **************************************/
public class ErnestModel extends Model 
{

	/** The angular field of each eye. */
	private double m_eyeAngle ;
	protected void setEyeAngle(double angle) {m_eyeAngle = angle;}
	protected double getEyeAngle() {return m_eyeAngle;}
	
	/** Ernest's sensorymotor system. */
	protected ArrayList<IErnest> m_ernest = new ArrayList<IErnest>();
	protected ISensorymotorSystem m_sensorymotorSystem;
	protected ITracer m_tracer;
	
	/**
	 * Initialize the Ernest agent.
	 */
	public void initErnest()
	{
	}
	
	public void closeErnest()
	{
		m_tracer.close();
		m_orientation = ORIENTATION_UP;
		m_ernest = null;
	}
	
	/**
	 * Run Ernest one step
	 */
	public String stepErnest(boolean status)
	{

		// Sense the environment
		int [][] matrix = new int [2][1];
		
		//m_sensorymotorSystem.senseMatrix(matrix);
		
		String intention = m_ernest.get(0).step(matrix);

		return intention;
	}
	
	/**
	 * Enact the primitive schema chosen by Ernest.
	 * @return binary feedback. 
	 */
	public boolean enactSchema(String schema)
	{
		m_schema = schema;
		
		System.out.println("Step #" + m_counter + "=======");
		m_tracer.startNewEvent(m_counter++);

		
		// Sucking the blue square is automatic
		if (isDirty()) suck();
		
		boolean status = false;
		
	    if (schema.equals("v"))
			status = turnRight();
		else if (schema.equals("^"))
			status = turnLeft();
		else if (schema.equals(">"))
			status = forward();
		else if (schema.equals("-"))
			status = touchForward(true);
		else if (schema.equals("\\"))
			status = touchRight(true);
		else if (schema.equals("/"))
			status = touchLeft(true);
		
		// Play Ernest's internal state
		if (m_ernest.get(0).internalState().equals("!"))
		{
			speak("Oh!", true, false);
			if (!getSpeakAloud())
				sleep(200);
		}
		else
			sleep(200);
		
	    return status;
	}
	
	/**
	 * Turn left. 
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnLeft()
	{
		m_eyeOrientation = 0;

		boolean status =  true  ;
		m_orientationAngle =  m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
		m_orientation -= getOrientationStep();
		double nextOrientationAngle =  m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
		int adjacent_x = (int)m_x;
		int adjacent_y = (int)m_y;

		if (m_orientation < ORIENTATION_UP)
			m_orientation = ORIENTATION_LEFT;

		// Adjacent square
		if (m_orientation == ORIENTATION_UP)
			adjacent_y = (int)m_y - 1;
		if (m_orientation == ORIENTATION_DOWN)
			adjacent_y = (int)m_y + 1;
		if (m_orientation == ORIENTATION_RIGHT)
			adjacent_x = (int)m_x + 1;
		if (m_orientation == ORIENTATION_LEFT)
			adjacent_x = (int)m_x - 1;

		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h))
		{
			if (isWall(adjacent_x, adjacent_y))
			{
				setAnim(adjacent_x,adjacent_y, ANIM_RUB); 
				status = false;
			}
		}
		else
			status = false;

		// Animation
		for (; m_orientationAngle >= nextOrientationAngle; m_orientationAngle -= .05)
		{
			setChanged();
			notifyObservers2();			
		}
		setAnim(adjacent_x, adjacent_y, ANIM_NO);
		setChanged();
		notifyObservers2();			
		
		return status;
	}
	
	/**
	 * Turn right.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnRight()
	{

		m_eyeOrientation = 0;
		
		boolean status =  true  ;
		m_orientationAngle = m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
		m_orientation += getOrientationStep();
		double nextOrientationAngle = m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
		int adjacent_x = (int)m_x;
		int adjacent_y = (int)m_y;
		
		if (m_orientation > ORIENTATION_LEFT)
			m_orientation = ORIENTATION_UP;
		
		// Adjacent square
		if (m_orientation == ORIENTATION_UP)
			adjacent_y = (int)m_y - 1;
		if (m_orientation == ORIENTATION_DOWN)
			adjacent_y = (int)m_y + 1;
		if (m_orientation == ORIENTATION_RIGHT)
			adjacent_x = (int)m_x + 1;
		if (m_orientation == ORIENTATION_LEFT)
			adjacent_x = (int)m_x - 1;

		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h))
		{
			if (isWall(adjacent_x, adjacent_y))
			{
				setAnim(adjacent_x,adjacent_y, ANIM_RUB); 
				status = false;
			}
		}
		else
			status = false;

		// Animation
		for (; m_orientationAngle <= nextOrientationAngle; m_orientationAngle += .05)
		{
			setChanged();
			notifyObservers2();			
		}
		setAnim(adjacent_x, adjacent_y, ANIM_NO);
		setChanged();
		notifyObservers2();			

		return status;
	}
	
	/**
	 * Move forward.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean forward()
	{

		boolean status = true;
		int adjacent_x = (int)m_x;
		int adjacent_y = (int)m_y;

		// Adjacent square
		if (m_orientation == ORIENTATION_UP)
			adjacent_y = (int)m_y - 1;
		if (m_orientation == ORIENTATION_DOWN)
			adjacent_y = (int)m_y + 1;
		if (m_orientation == ORIENTATION_RIGHT)
			adjacent_x = (int)m_x + 1;
		if (m_orientation == ORIENTATION_LEFT)
			adjacent_x = (int)m_x - 1;

		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h))
		{
			if (isWall(adjacent_x, adjacent_y))
			{
				setAnim(adjacent_x,adjacent_y, ANIM_BUMP); 
				status = false;
			}
			else
			{
				m_x = adjacent_x;
				m_y = adjacent_y;
			}
		}
		else
			status = false;

		// Animation
		setChanged();
		notifyObservers2();			
			
		sleep(200);
		setAnim(adjacent_x, adjacent_y, ANIM_NO);
		setChanged();
		notifyObservers2();			

		return status;
	}

	/**
	 * Touch the square forward.
	 * @return true if wall, false if empty. 
	 */
	protected boolean touchForward(boolean flash)
	{

		boolean status = true;

		if (m_orientation == ORIENTATION_UP)
		{
			if (( m_y > 0 ) && flash) 
				setAnim(m_x,m_y - 1, ANIM_TOUCH); 
		    if ((m_y > 0) && !isWall(m_x, m_y - 1))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_DOWN)
		{
			if (( m_y < m_h ) && flash)  
				setAnim(m_x,m_y + 1, ANIM_TOUCH); 
			if ((m_y < m_h) && !isWall(m_x, m_y + 1))
				status = false;
		}
		if (m_orientation == ORIENTATION_RIGHT)
		{
			if (( m_x < m_w ) && flash) 
				setAnim(m_x + 1,m_y, ANIM_TOUCH); 
		    if ((m_x < m_w) && !isWall(m_x + 1, m_y))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_LEFT)
		{
			if (( m_x > 0 ) && flash) 
				setAnim(m_x - 1,m_y, ANIM_TOUCH); 
		    if ((m_x > 0) && !isWall(m_x - 1, m_y))
		    	status = false;
		}

		setChanged();
		if(flash)
			notifyObservers2();
		
		return status;
	}

	/**
	 * Touch to the right
	 * @return true if wall, false if empty. 
	 */
	protected boolean touchRight(boolean flash)
	{

		boolean status = true;

		if (m_orientation == ORIENTATION_LEFT)
		{
			if (( m_y > 0 ) && flash) 
				setAnim(m_x,m_y - 1, ANIM_TOUCH); 
		    if ((m_y > 0) && !isWall(m_x, m_y - 1))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_RIGHT)
		{
			if (( m_y < m_h ) && flash) 
				setAnim(m_x,m_y + 1, ANIM_TOUCH); 
			if ((m_y < m_h) && !isWall(m_x, m_y + 1))
				status = false;
		}
		if (m_orientation == ORIENTATION_UP)
		{
			if (( m_x < m_w ) && flash) 
				setAnim(m_x + 1,m_y, ANIM_TOUCH); 
		    if ((m_x < m_w) && !isWall(m_x + 1, m_y))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_DOWN)
		{
			if (( m_x > 0 ) && flash) 
				setAnim(m_x - 1,m_y, ANIM_TOUCH); 
		    if ((m_x > 0) && !isWall(m_x - 1, m_y))
		    	status = false;
		}
		
		setChanged();
		if(flash)
			notifyObservers2();
		
		return status;
	}

	/**
	 * Touch to the left
	 * @return true if wall, false if empty. 
	 */
	protected boolean touchLeft(boolean flash)
	{

		boolean status = true;

		if (m_orientation == ORIENTATION_RIGHT)
		{
			if (( m_y > 0 ) && flash) 
				setAnim(m_x,m_y - 1, ANIM_TOUCH); 
		    if ((m_y > 0) && !isWall(m_x, m_y - 1))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_LEFT)
		{
			if (( m_y < m_h ) && flash) 
				setAnim(m_x,m_y + 1, ANIM_TOUCH); 
			if ((m_y < m_h) && !isWall(m_x, m_y + 1))
				status = false;
		}
		if (m_orientation == ORIENTATION_DOWN)
		{
			if (( m_x < m_w ) && flash) 
				setAnim(m_x + 1,m_y, ANIM_TOUCH);
		    if ((m_x < m_w) && !isWall(m_x + 1, m_y))
		    	status = false;
		}
		if (m_orientation == ORIENTATION_UP)
		{
			if (( m_x > 0 ) && flash) 
				setAnim(m_x - 1,m_y, ANIM_TOUCH); 
		    if ((m_x > 0) && !isWall(m_x - 1, m_y))
		    	status = false;
		}
		
		setChanged();
		if(flash)
			notifyObservers2();
		
		return status;
	}

	/**
	 * Trace an event generated by the user, typically: a mouse click to change a square on the grid.
	 * @param type The event's type.
	 * @param x The x coordinate on the grid.
	 * @param y The y coordinate on the grid.
	 */
	public void traceUserEvent(String type, int x, int y)
	{
		//Object element = m_tracer.newEvent("user", type, m_counter);
		//m_tracer.addSubelement(element, "x", x + "");
		//m_tracer.addSubelement(element, "y", y + "");
	}
	
	/**
	 * Paint Ernest as a horseshoe crab.
	 * @param g The graphic object for painting.
	 */
	public void paintAgent(Graphics2D g2d)
	{
		// The orientation
		AffineTransform orientation = new AffineTransform();
		orientation.rotate(m_orientationAngle);
		g2d.transform(orientation);
				
		// The vision
		//Color leftEyeColor = visualLeft().getColor();
		//Color rightEyeColor = visualRight().getColor();

		

		AffineTransform bodyReference = g2d.getTransform();
		
		// Define the graphic objects
		
		// Body
		Ellipse2D.Double cephalothorax = new Ellipse2D.Double(-25, -25, 50, 50); 
		Polygon abdomen = new Polygon();	
		abdomen.addPoint(12, 5); abdomen.addPoint(15, 20); abdomen.addPoint(6, 30); 
		abdomen.addPoint(-6, 30); abdomen.addPoint(-15, 20);abdomen.addPoint(-12, 5);
		Polygon tail = new Polygon();	tail.addPoint(-5, 22); tail.addPoint(5, 22); tail.addPoint(0, 40);
		Arc2D.Double eyelid = new Arc2D.Double(0, 0, 40, 40,0, 90, Arc2D.PIE);

		// Eyes
		Arc2D.Double eye = new Arc2D.Double(0, 0, 38, 38,0, getEyeAngle() * 180/Math.PI, Arc2D.PIE);
		
		// Draw the body
		
		Color fill = new Color(200, 100, 100);
		Color line = new Color(230, 200, 200);
		Color lidColor = new Color(150, 30, 30);
		lidColor = line;
		g2d.setStroke(new BasicStroke(2f));

		g2d.setColor(fill); g2d.fill(cephalothorax);
		g2d.setColor(line);	g2d.draw(cephalothorax);

		g2d.setColor(fill); g2d.fill(abdomen);
		g2d.setColor(line); g2d.draw(abdomen);

		g2d.setColor(fill);	g2d.fill(tail);
		g2d.setColor(line);	g2d.draw(tail);
				
		// Draw the left eye
		AffineTransform transformLeftEye = new AffineTransform();
		transformLeftEye.translate(-25, -30);
		transformLeftEye.rotate(- Math.PI/2, 20, 20);
		g2d.transform(transformLeftEye);

		g2d.setColor(lidColor);	g2d.fill(eyelid);
		g2d.setStroke(new BasicStroke(1f));
		g2d.setColor(fill);	g2d.draw(eyelid);

		transformLeftEye.setToIdentity();
		transformLeftEye.translate(0, 0);
		g2d.transform(transformLeftEye);

		//g2d.setColor(leftEyeColor);g2d.fill(eye);
		g2d.setStroke(new BasicStroke(1f));

		g2d.setTransform(bodyReference);

		// Draw the right eye
		AffineTransform TransformRightEye = new AffineTransform();
		TransformRightEye.translate(-15, -30);
		g2d.transform(TransformRightEye);

		g2d.setColor(lidColor);	g2d.fill(eyelid);
		g2d.setColor(fill); g2d.draw(eyelid);

		TransformRightEye.setToIdentity();
		TransformRightEye.translate(2, 0);
		TransformRightEye.rotate(getEyeAngle() - Math.PI/2, 20, 20);
		g2d.transform(TransformRightEye);

		//g2d.setColor(rightEyeColor);g2d.fill(eye);
		g2d.setStroke(new BasicStroke(1f));
		
	}

}
