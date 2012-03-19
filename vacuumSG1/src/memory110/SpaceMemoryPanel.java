package memory110;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;
import javax.vecmath.Vector3f;

import agent.Ernest110Model;

import ernest.Ernest;
import ernest.IErnest;

import spas.IAffordance;
import spas.IPlace;
import spas.LocalSpaceMemory;
import spas.Spas;
import utils.ErnestUtils;



public class SpaceMemoryPanel extends JPanel
{
	/** The radius of the display area in grid units. */
	public final static int WIDTH = 300;
	public final static int HEIGHT = 250;
	
	/** The number of pixels per grid units. */
	public final static int SCALE = 35;//40; 
	
	private static final long serialVersionUID = 1L;
	public int index;
	
	//public ArrayList<IPlace> placeList;
	
	private IErnest m_ernest;
	
	public SpaceMemory spaceMemory;
	
	public SpaceMemoryPanel(SpaceMemory spaceMemory2){
		index=0;
		//placeList=new ArrayList<IPlace>();
		spaceMemory=spaceMemory2;
	}
	
	public void setMemory(SpaceMemory mem){
		spaceMemory=mem;
	}
	
	public void paintComponent(Graphics g)
	{
		boolean displaySensePlace = true;
		
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

		// Display background
		g2d.setColor(Color.white);
		//g2d.fillRect(0, 0, 2 * RADIUS * SCALE, 2 * RADIUS * SCALE);
		
        // Display counter
		String counter = spaceMemory.getCounter() + ""; 
		Font font = new Font("Dialog", Font.BOLD, 18);
		g2d.setFont(font);
		FontMetrics fm = getFontMetrics(font);
		int width = fm.stringWidth(counter);
		g2d.setColor(Color.GRAY);		
		g2d.drawString(counter, 2 * WIDTH - 30 - width, 30);	
		
		float agentOrientation = 0;
//		for (IPlace place : spaceMemory.getPlaceList())
//		{
//			if (place.getType() == Spas.PLACE_FOCUS)
//			{
//				refAngle = place.getDirection();
//			}
//		}

		IPlace focusPlace = spaceMemory.getFocusPlace();
		agentOrientation = spaceMemory.getOrientation();
		
		//float baseOrientation = - agentOrientation + focusPlace.getOrientation();
		float baseOrientation = agentOrientation; // allocentric
		//float baseOrientation = - focusPlace.getOrientation(); // target centric.
		//float baseOrientation = 0; // agent horizontal
		float x = (float)Math.cos(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		float y = (float)Math.sin(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		
		AffineTransform ref0 = g2d.getTransform();
		AffineTransform ref1 = new AffineTransform();
		//ref1.translate( - x * SCALE,  y * SCALE);
        //ref1.rotate(- baseOrientation, WIDTH, HEIGHT);
        g2d.transform(ref1);
        //g2d.setTransform(ref0);
		
		// Display agent
        AffineTransform ref = g2d.getTransform();
//        AffineTransform placeAgent = new AffineTransform();
//        placeAgent.translate(WIDTH, HEIGHT);
//        placeAgent.rotate(Math.PI/2);
//        placeAgent.scale(SCALE / 100f, SCALE / 100f);
//        g2d.transform(placeAgent);
//		g2d.setColor(Color.gray);
//        g2d.fill(Ernest110Model.shape(spaceMemory.getID()));
        
        g2d.setTransform(ref);
        
		double d;
		double rad;
		double angle;
		double span;
		
        // Display the places
		//g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
		g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER));

		Ellipse2D.Double circle = new Ellipse2D.Double(-10, -10, 20, 20); 
        Arc2D.Double pie = new Arc2D.Double(-10, -10, 20, 20,0, 180, Arc2D.PIE);
        Polygon triangle = new Polygon();triangle.addPoint(-10,-10);triangle.addPoint(-10,10);triangle.addPoint(10,0);

        if (displaySensePlace)
        {
			// Display the visual and tactile places
			for (IPlace place : spaceMemory.getPlaceList())
			{
				//if (place.getType() == Spas.PLACE_PERSISTENT)
				if (place.getType() < Spas.PLACE_FOCUS && place.getType() > Spas.PLACE_BACKGROUND)
				{
					d = place.getPosition().length() * SCALE;
					
					rad = (float)Math.atan2((double)place.getPosition().y, place.getPosition().x);			
					//rad = (float)Math.atan2((double)place.getFirstPosition().y, place.getFirstPosition().x);			
					angle = rad*180/Math.PI;
								
					span=place.getSpan()*180/Math.PI;
					g2d.setColor(new Color(place.getBundle().getValue()));		
					
					//g2d.setStroke(new BasicStroke(SCALE / (3f + 2*(spaceMemory.getUpdateCount() - place.getUpdateCount())), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
					g2d.setStroke(new BasicStroke(Math.max(SCALE / 4f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
		
					if (place.getType() == Spas.PLACE_TOUCH)
						//g2d.drawArc(WIDTH - (int)d, HEIGHT - (int)d, 2*(int)d, 2*(int)d, (int)(angle), (int)1);
						g2d.drawArc(WIDTH - (int)d, HEIGHT - (int)d, 2*(int)d, 2*(int)d, (int)(ErnestUtils.polarAngle(place.getFirstPosition())*180/Math.PI), (int)(place.getSpan()*180/Math.PI));
					else
						g2d.drawLine(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE), 
							WIDTH + (int)(place.getSecondPosition().x * SCALE), HEIGHT - (int)(place.getSecondPosition().y * SCALE));
					
					// Display the affordances 
					if (place == focusPlace)
					{
						float absoluteOrientation = - place.getOrientation() ; 
						AffineTransform ref2 = g2d.getTransform();
						AffineTransform local = new AffineTransform();
				        local.rotate(absoluteOrientation, WIDTH + (int)(place.getPosition().x * SCALE),HEIGHT - (int)(place.getPosition().y * SCALE) );
				        g2d.transform(local);
	
						g2d.setStroke(new BasicStroke(SCALE / 10f));
						AffineTransform or;
						for (IAffordance affordance : place.getBundle().getAffordanceList())
						{
							int x2 = (int)((place.getPosition().x + affordance.getPlace().getPosition().x) * SCALE); 
							int y2 = (int)((place.getPosition().y + affordance.getPlace().getPosition().y) * SCALE); 
		 					
							Shape shape = circle;
							if (affordance.getPlace().getShape() == Spas.SHAPE_TRIANGLE)
								shape = triangle;
							else if (affordance.getPlace().getShape() == Spas.SHAPE_PIE)
								shape = pie;
							
					        ref = g2d.getTransform();
					        or = new AffineTransform();
					        or.translate(WIDTH + x2, HEIGHT - y2);
					        or.scale(( .6f  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( .6f  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
					        or.rotate(- affordance.getPlace().getOrientation());
					        g2d.transform(or);
							g2d.setColor(new Color(affordance.getValue()));
							g2d.fill(shape);
							g2d.setColor(new Color(place.getBundle().getValue()));
							g2d.draw(shape);
					        g2d.setTransform(ref);
							//g2d.fillOval(WIDTH + x2 - 5, HEIGHT - y2 + 5, 10, 10);
						}
				        g2d.setTransform(ref2);
					}
				}			
			}
        }
		
		// Display the bump, eat, and cuddle places
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		AffineTransform or;
		for (IPlace place : spaceMemory.getPlaceList())
		{
			if (place.getType() >= Spas.PLACE_FOCUS)// && place.getType() < Spas.PLACE_PERSISTENT)
			{
				// The places represented as arcs
				//g2d.setColor(new Color(place.getBundle().getValue()));		
				if (place.getType() == Spas.PLACE_BUMP) 
					g2d.setColor(Color.RED);
				if (place.getType() == Spas.PLACE_EAT )
					g2d.setColor(Color.YELLOW);
				if (place.getType() == Spas.PLACE_CUDDLE) 
					g2d.setColor(Color.PINK);
				if (place.getType() == Spas.PLACE_PRIMITIVE) 
					g2d.setColor(Color.BLUE);
				if (place.getType() == Spas.PLACE_COMPOSITE) 
					g2d.setColor(new Color(0, 0, 128));
				if (place.getType() == Spas.PLACE_INTERMEDIARY) 
					g2d.setColor(new Color(128, 128, 255));
				if (place.getType() == Spas.PLACE_FOCUS) 
					g2d.setColor(Color.MAGENTA);
				
				Shape shape = circle;
				if (place.getShape() == Spas.SHAPE_TRIANGLE)
					shape = triangle;
				else if (place.getShape() == Spas.SHAPE_PIE)
					shape = pie;
				
		        ref = g2d.getTransform();
		        or = new AffineTransform();
		        or.translate(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE));
		        or.scale(( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
		        or.rotate(- place.getOrientation());
		        g2d.transform(or);
				g2d.fill(shape);
		        g2d.setTransform(ref);
				//g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
	
				//g2d.drawLine(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE), 
				//	WIDTH + (int)(place.getSecondPosition().x * SCALE), HEIGHT - (int)(place.getSecondPosition().y * SCALE));
			}
		}
				
		// Display the focus
		int focusRadius = SCALE / 5;
		g2d.setStroke(new BasicStroke(SCALE / 10f));

		d = focusPlace.getPosition().length() * SCALE;
		rad = (float)Math.atan2((double)focusPlace.getPosition().y, focusPlace.getPosition().x);			
		g2d.setColor(new Color(focusPlace.getBundle().getValue()));		
		int x0 = WIDTH + (int) (d * Math.cos(rad));
		int y0 = HEIGHT - (int) (d * Math.sin(rad));
		//g2d.fillOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
		
		g2d.setColor(Color.MAGENTA);	
		//g2d.setColor(new Color(focusPlace.getBundle().getValue()));
		if (focusPlace.getSpeed() != null)
			g2d.drawLine(x0, y0, x0 + (int)(focusPlace.getSpeed().x * SCALE), y0 - (int)(focusPlace.getSpeed().y * SCALE));
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		g2d.drawOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
		//g2d.fillOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
		//g2d.setColor(Color.BLUE);	
		float absoluteOrientation = focusPlace.getOrientation();// + agentOrientation; 
		//g2d.drawLine(x0 - (int)(Math.cos(absoluteOrientation) * focusRadius), y0 + (int)(Math.sin(absoluteOrientation) * focusRadius), x0 + (int)(Math.cos(absoluteOrientation) * focusRadius *2), y0 - (int)(Math.sin(absoluteOrientation) * focusRadius *2));

		// Display agent
        //AffineTransform ref = g2d.getTransform();
        g2d.setTransform(ref);
        AffineTransform placeAgent = new AffineTransform();
        placeAgent.translate(WIDTH, HEIGHT);
        placeAgent.rotate(Math.PI/2);
        placeAgent.scale(SCALE / 100f, SCALE / 100f);
        g2d.transform(placeAgent);
		g2d.setColor(Color.gray);
        g2d.fill(Ernest110Model.shape(spaceMemory.getID()));
        
	}
}
