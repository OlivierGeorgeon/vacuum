package memory110;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.GeneralPath;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;

import agent.Ernest110Model;

import ernest.Ernest;

import spas.IPlace;



public class SpaceMemoryPanel extends JPanel
{
	/** The radius of the display area in grid units. */
	public final static int RADIUS = 6;
	
	/** The number of pixels per grid units. */
	public final static int SCALE = 40; 
	
	private static final long serialVersionUID = 1L;
	public int index;
	
	public ArrayList<IPlace> placeList;
	
	public SpaceMemory spaceMemory;
	
	public SpaceMemoryPanel(SpaceMemory spaceMemory2){
		index=0;
		placeList=new ArrayList<IPlace>();
		spaceMemory=spaceMemory2;
	}
	
	public void setMemory(SpaceMemory mem){
		spaceMemory=mem;
	}
	
	public void update(ArrayList<IPlace> list){
		placeList=list;
	}
	
	public void paintComponent(Graphics g){
		
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
		g2d.setColor(Color.white);
		g2d.fillRect(0, 0, 2 * RADIUS * SCALE, 2 * RADIUS * SCALE);
		
        AffineTransform ref = g2d.getTransform();
        AffineTransform orientation = new AffineTransform();
        orientation.translate(RADIUS * SCALE,RADIUS * SCALE);
        orientation.rotate(Math.PI/2);
        orientation.scale(SCALE / 100f, SCALE / 100f);
        g2d.transform(orientation);

        // display agent
		g2d.setColor(Color.gray);
        g2d.fill(Ernest110Model.shape());
		
        g2d.setTransform(ref);

        // Transparency
//		float alpha = 0.8f;
//		int type = AlphaComposite.SRC_OVER; 
//		AlphaComposite composite = AlphaComposite.getInstance(type, alpha);
//		g2d.setComposite(composite);
		
		double d;
		double rad;
		double angle;
		double span;
		int focusRadius = SCALE / 3;
		
		g2d.setStroke(new BasicStroke(SCALE / 3f));
		
		for (IPlace place : spaceMemory.placeList)
		{
			d = place.getPosition().length() * SCALE;
			
			rad = (float)Math.atan2((double)place.getPosition().y, place.getPosition().x);			
			angle = rad*180/Math.PI;
						
			// The places represented as arcs
			span=place.getSpan()*180/Math.PI;
			g2d.setColor(new Color(place.getBundle().getValue()));			
			g2d.drawArc(RADIUS * SCALE - (int)d, RADIUS * SCALE - (int)d, 2*(int)d, 2*(int)d, (int)(angle-span/2), (int)span);
			
			// The focus represented as a red circle
			if (place.getFocus())
			{
				if (place.getAttractiveness(1) >= 0)
					g2d.setColor(Color.MAGENTA);			
				else
					g2d.setColor(Color.RED);			
				g2d.fillOval(RADIUS * SCALE + (int) (d * Math.cos(rad)) - focusRadius, RADIUS * SCALE  - (int) (d * Math.sin(rad)) - focusRadius, 2 * focusRadius, 2 * focusRadius);
			}
		}
	}
}
