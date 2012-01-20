package memory110;

import java.util.ArrayList;

import ernest.IErnest;

import spas.IPlace;

public class SpaceMemory {

	public IErnest m_ernest;

	public ArrayList<IPlace> placeList;
	public int m_counter;
	public int m_focus;
	
	public SpaceMemory(){
	}
	
	public void setErnest(IErnest ernest)
	{	
		m_ernest = ernest;
	}
	
	/**
	 * Update the the information to display
	 */
	public void update()
	{
		placeList = m_ernest.getPlaceList();
		m_counter = m_ernest.getCounter();
		m_focus = m_ernest.getAttention();
	}
	
}
