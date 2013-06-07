package ideal.vacuum.view;

import ideal.vacuum.ErnestModel ;

import java.util.ArrayList;

import javax.vecmath.Vector3f;


import ernest.IErnest;

import spas.Place;

public class SpaceMemory 
{
	public ErnestModel m_model;
	private ArrayList<Place> placeList = new ArrayList<Place>();
	
	public SpaceMemory()
	{
	}
	
	public void setModel(ErnestModel model)
	{	
		m_model = model;
	}
	
	public ArrayList<Place> getPlaceList()
	{
		//return m_model.getErnest().getPlaceList();
		return this.placeList;
	}
	
	public void setPlaceList()
	{
		this.placeList =  m_model.getErnest().getPlaceList();
	}
	
	public int getCounter()
	{
		return m_model.getErnest().getCounter();
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
	
}
