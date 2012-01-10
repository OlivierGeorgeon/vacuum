package memory110;

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
	final static int SCALE = 50; 
	final static int RADIUS = 5;
	
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
		
		int size=placeList.size();
		
		double d;
		double angle;
		double span;
		
		g2d.setStroke(new BasicStroke(10.0f));
		
		for (int i=0;i<size;i++){
			
			d = placeList.get(i).getPosition().length() * SCALE;
			
			angle = (float)Math.atan2((double)placeList.get(i).getPosition().y, placeList.get(i).getPosition().x);
			
			angle=angle*180/Math.PI;
			span=placeList.get(i).getSpan()*180/Math.PI;
			
			g2d.setColor(new Color(placeList.get(i).getBundle().getValue()));			
			g2d.drawArc(RADIUS * SCALE-(int)d, RADIUS * SCALE -(int)d, 2*(int)d, 2*(int)d, (int)(angle-span/2), (int)span);
			
		}
	}
}
