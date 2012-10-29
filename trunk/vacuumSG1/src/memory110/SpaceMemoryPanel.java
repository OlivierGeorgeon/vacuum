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

//import agent.Ernest110Model;
import agent.PrintablePanel;

import ernest.IErnest;




public class SpaceMemoryPanel extends PrintablePanel
{
	private static final long serialVersionUID = 1L;
	
	//public int index;
	//private IErnest m_ernest;
	
	public SpaceMemory spaceMemory;
	
	public SpaceMemoryPanel(SpaceMemory mem){
		//index=0;
		
		spaceMemory = mem;
	}
	
	public void setMemory(SpaceMemory mem){
		spaceMemory = mem;
	}
	
	public void paintComponent(Graphics g)
	{
//		spaceMemory.m_model.paintSpaceMemory(g);   
		spaceMemory.m_model.paintSpaceMemory(g, spaceMemory.m_model.getErnest().getPlaceList());   
	}
}
