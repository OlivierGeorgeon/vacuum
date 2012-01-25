package memory110;

import java.util.ArrayList;

import agent.ErnestModel;

import ernest.IErnest;

import spas.IPlace;

public class SpaceMemory 
{
	public IErnest m_ernest;
	public ErnestModel m_model;
 
	public ArrayList<IPlace> placeList;
	public int m_counter;
	public int m_focus;
	public int m_id;
	public boolean lock=false;
	
	public SpaceMemory()
	{
	}
	
	public void setErnest(IErnest ernest)
	{	
		m_ernest = ernest;
	}
	
	public void setModel(ErnestModel model)
	{	
		m_model = model;
	}
	
	/**
	 * Update the the information to display
	 */
	public void update()
	{
		lock=false;
		placeList = m_ernest.getPlaceList();
		m_counter = m_ernest.getCounter();
		m_focus = m_ernest.getAttention();
		m_id = m_model.getID();
		lock=true;
	}
	
}
