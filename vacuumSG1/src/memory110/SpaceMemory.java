package memory110;

import java.util.ArrayList;

import javax.vecmath.Vector3f;

import agent.ErnestModel;

import ernest.IErnest;

import spas.IPlace;

public class SpaceMemory 
{
	public ErnestModel m_model;
//	public boolean lock=false;
	
	public SpaceMemory()
	{
	}
	
	public void setModel(ErnestModel model)
	{	
		m_model = model;
	}
	
	/**
	 * Update the information to display
	 */
//	public void update()
//	{
//		lock=false;
////		placeList = m_ernest.getPlaceList();
////		m_counter = m_ernest.getCounter();
////		m_focus = m_ernest.getAttention();
////		m_id = m_model.getID();
//		lock=true;
//	}
	
	public ArrayList<IPlace> getPlaceList()
	{
		return m_model.getErnest().getPlaceList();
	}
	
	public int getCounter()
	{
		return m_model.getErnest().getCounter();
	}
	
	public int getFocus()
	{
		return m_model.getErnest().getAttention();
	}
	
	public int getID()
	{	
		return m_model.getID();
	}

	public int getUpdateCount()
	{
		return m_model.getUpdateCount();
	}
	
	public float getOrientation()
	{
		return m_model.getPreviousOrientation();
	}
	
//	public IPlace getFocusPlace()
//	{
//		return m_model.getErnest().getFocusPlace();
//	}	
}
