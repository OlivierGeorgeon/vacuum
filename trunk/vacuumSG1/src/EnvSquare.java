

import java.awt.*;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.geom.AffineTransform;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;

import java.awt.geom.*;


import javax.swing.*;

/**
 * Represent a square in the environment.
 * @author mcohen
 * @author ogeorgeon
 */
public class EnvSquare extends JPanel
{
	public static final long serialVersionUID = 1;
	
	private int m_x;
	private int m_y;
	private Model m_model;
	private int m_clicked = 0;

	/**
	 * Constuctor. 
	 * Activates a mouse listener.
	 * @param x The square's x coordinate
	 * @param y The square's y coordinate (goes downward)
	 * @param model The model that can tell what to display in this square
	 */
	public EnvSquare(int x, int y, Model model)
	{
		m_x = x;
		m_y = y;
		m_model = model;
	}

	/**
	 * Update the square's background.
	 * This triggers the square repaint and reset the possible animations.
	 */
	public void udateBackground()
	{
		setBackground(m_model.getBackgroundColor(m_x, m_y));
		//m_model.setAnim(m_x, m_y, Model.ANIM_NO);
	}
	
	/**
	 * Paint the square.
	 */
	public void paint(Graphics g)
	{
		super.paint(g);
		
		Graphics2D g2d = (Graphics2D)g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
		AffineTransform squareReference = g2d.getTransform();
		
		Polygon hive = new Polygon();
		hive.addPoint(30, -10);hive.addPoint(0, -30);hive.addPoint(-30, -10);hive.addPoint(-30, 30);hive.addPoint(30, 30);

		CubicCurve2D.Double petal1 = new CubicCurve2D.Double(0, 0,  0,80, 80,0,   0, 0);
		CubicCurve2D.Double petal2 = new CubicCurve2D.Double(0, 0, 80, 0,  0,-80, 0, 0);
		CubicCurve2D.Double petal3 = new CubicCurve2D.Double(0, 0,  0,-80,-80, 0, 0, 0);
		CubicCurve2D.Double petal4 = new CubicCurve2D.Double(0, 0, -80, 0, 0, 80, 0, 0);

		GeneralPath fish = new GeneralPath();
		fish.append(new CubicCurve2D.Double(-40, 15,  -30, 0, 40, -40,   40, 0), false);
		fish.append(new CubicCurve2D.Double( 40,  0,  40, 40, -30,  0,   -40, -15), true);

		// Set the transformation to generate the display from the design space.
		AffineTransform centerSquare = new AffineTransform();
		centerSquare.translate(getWidth()/2, getHeight()/2);
		centerSquare.scale((float)getWidth()/90, (float)getWidth()/90); 
		g2d.transform(centerSquare);

		// Masks
		g2d.setColor(Color.WHITE);
		Area maskSingularity = new Area(new Rectangle2D.Double(-50, -50, 100, 100));
		if (m_model.getDirty(m_x, m_y) == m_model.DIRTY )
		{
			Area fishArea = new Area(fish);
			fishArea.subtract(new Area(new Ellipse2D.Double( 20,  -10,  8, 8)));
			maskSingularity.subtract(fishArea);
			g2d.fill(maskSingularity);
		}
		if (m_model.getDirty(m_x, m_y) > m_model.DIRTY)
		{
			maskSingularity.subtract(new Area(petal1));
			maskSingularity.subtract(new Area(petal2));
			maskSingularity.subtract(new Area(petal3));
			maskSingularity.subtract(new Area(petal4));			
			g2d.fill(maskSingularity);
		}
		
		// The night
		
		if (m_model.isNight())
		{
			AlphaComposite toto = AlphaComposite.getInstance(AlphaComposite.SRC_OVER, 0.7f);
			Composite originalComposite = g2d.getComposite();
			g2d.setComposite(toto);
			g2d.setPaint(Color.BLACK);
			g2d.fillRect(-100, -100, 4 * getWidth(), 4 * getHeight());	
			g2d.setComposite(originalComposite);
		}

		// The information square
		//if (m_model.isInformation(m_x, m_y))
		//if (m_model.getWall(m_x, m_y) == Model.WALL_INFORMATION )
			//drawInformation(g2d);
		
		// The dream square
		if (m_model.getWall(m_x, m_y) == Model.WALL_INFORMATION2 )
			m_model.paintDream(g2d);
		
		// The agent
		if (m_model.isAgent(m_x, m_y))
			m_model.paintAgent(g2d);
		
		// The square outline
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_OFF);
		g2d.setTransform(squareReference);
		g2d.setStroke(new BasicStroke(1f));g2d.setColor(Color.BLACK);
		g2d.drawRect(0, 0, getWidth(), getHeight());
	
	}
	
	/**
	 * Draw the information square.
	 */
	private void drawInformation(Graphics2D g2d)
	{
		String counter = m_model.getCounter() + ""; 
		
		Font font = new Font("Dialog", Font.BOLD, 24);
		g2d.setFont(font);
		
		FontMetrics fm = getFontMetrics(font);
		int width = fm.stringWidth(counter);

		g2d.setColor(new Color(200, 255, 200));		
		g2d.drawString(counter, 30 - width, 10);	
	}


}
		
