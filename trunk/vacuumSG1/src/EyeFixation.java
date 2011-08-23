

import java.awt.Color;

/**************************************
 * The result of an eye fixation.
 * For Ernest 8.4 and 9.0: contains the distance, the color perceived, and the indication of singularity. 
 * @author ogeorgeon
 **************************************/
public class EyeFixation 
{
	private int m_distance ;
	private Color m_color;
	private double m_angle = 0;
	
	private Color m_color2;
	private int m_distance2;
	
	public void setAngle(double angle)
	{
		m_angle = angle;
	}

	public double getAngle()
	{
		return m_angle;
	}
	public void setDistance(int distance)
	{
		m_distance = distance;
	}
	
	public int getDistance()
	{
		return m_distance;
	}
	
	public void setColor(Color color)
	{
		m_color = color;
	}
	
	public Color getColor()
	{
		return m_color;
	}
	
	public String getHexColor()
	{
		String s = String.format("%06X", m_color.getRGB()  & 0x00ffffff); 
		return s;
	}

	public void setDistance2(int distance)
	{
		m_distance2 = distance;
	}
	
	public int getDistance2()
	{
		return m_distance2;
	}
	
	public void setColor2(Color color)
	{
		m_color2 = color;
	}
	
	public Color getColor2()
	{
		return m_color2;
	}
	
	public String getHexColor2()
	{
		String s = String.format("%06X", m_color2.getRGB()  & 0x00ffffff); 
		return s;
	}

}
