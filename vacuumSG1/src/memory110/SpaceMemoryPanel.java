package memory110;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.util.ArrayList;
import java.util.List;

import javax.swing.JPanel;

import spas.IPlace;



public class SpaceMemoryPanel extends JPanel{
	
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
		g2d.fillRect(0, 0, 800, 800);
		
		// display agent
		g2d.setColor(Color.gray);
		g2d.fillOval(350, 350, 100, 100);
		
		int size=placeList.size();
		
		double x,y;
		int rgb=0;
		int red,green,blue;
		double d;
		double angle;
		double span;
		
		//g.drawArc(100, 100, 100, 100, 0, 90);
		//g.drawArc(100, 100, 100, 100, 180, 90);
		g2d.setStroke(new BasicStroke(10.0f));
		
		for (int i=0;i<size;i++){
			
			x=placeList.get(i).getPosition().x*100;
			y=placeList.get(i).getPosition().y*100;
			
			rgb=placeList.get(i).getBundle().getValue();
			red=(int)(rgb/65536);
			green=(int)((rgb-red*65536)/256);
			blue=(int)(rgb-red*65536-green*256);
			
			d=Math.sqrt(x*x+y*y);
			
			if (x>0) angle=Math.atan(y/x);
			else{
				if (x<0) angle=Math.atan(y/x)+Math.PI;
				else{
					if (y>=0) angle=  Math.PI/2;
					else      angle=3*Math.PI/2;
				}
			}
			
			angle=angle*180/Math.PI;
			span=placeList.get(i).getSpan()*180/Math.PI;
			
			
			g2d.setColor(new Color(red,green,blue));
			
			
			//g.fillOval(400+(int)x,400-(int)y, 10, 10);
			
			g2d.drawArc(400-(int)d, 400-(int)d, 2*(int)d, 2*(int)d, (int)(angle-span/2), (int)span);
			
		}
	}
	
	
}