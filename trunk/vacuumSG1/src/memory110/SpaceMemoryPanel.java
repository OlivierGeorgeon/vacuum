package memory110;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;

import spas.IPlace;



public class SpaceMemoryPanel extends JPanel
{
	public final static int SCALE = 50; 
	public final static int RADIUS = 5;
	
	private static final long serialVersionUID = 1L;
	public int index;
	
	public ArrayList<IPlace> placeList;
	
	public SpaceMemory spaceMemory;
	
	public SpaceMemoryPanel(){
		index=0;
		placeList=new ArrayList<IPlace>();
	}
	
	public void setMemory(SpaceMemory mem){
		spaceMemory=mem;
	}
	
	public void update(ArrayList<IPlace> list){
		placeList=list;
	}
	
	public void paintComponent(Graphics g){
		
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setColor(Color.white);
		g2d.fillRect(0, 0, 2 * RADIUS * SCALE, 2 * RADIUS * SCALE);
		
		// display agent
		g2d.setColor(Color.gray);
		g2d.fillOval(RADIUS * SCALE - SCALE/2 , RADIUS * SCALE - SCALE/2, SCALE, SCALE);
		
		// Transparency
//		float alpha = 0.8f;
//		int type = AlphaComposite.SRC_OVER; 
//		AlphaComposite composite = AlphaComposite.getInstance(type, alpha);
//		g2d.setComposite(composite);
		
		double d;
		double angle;
		double span;
		
		g2d.setStroke(new BasicStroke(10.0f));
		
		for (IPlace place : placeList)
		{
			d = place.getPosition().length() * SCALE;
			
			angle = (float)Math.atan2((double)place.getPosition().y, place.getPosition().x);			
			angle=angle*180/Math.PI;
			
			span=place.getSpan()*180/Math.PI;
			
			g2d.setColor(new Color(place.getBundle().getValue()));			
			g2d.drawArc(RADIUS * SCALE-(int)d, RADIUS * SCALE -(int)d, 2*(int)d, 2*(int)d, (int)(angle-span/2), (int)span);
		}
	}
}
