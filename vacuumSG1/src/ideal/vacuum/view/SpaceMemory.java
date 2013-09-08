package ideal.vacuum.view;

import ideal.vacuum.ErnestModel ;

import java.util.ArrayList;

import javax.vecmath.Vector3f;


import eca.ActInstance;
import eca.spas.Placeable;
import ernest.IErnest;


public class SpaceMemory 
{
	public ErnestModel m_model;
	private ArrayList<Placeable> placeList = new ArrayList<Placeable>();
	
	public SpaceMemory()
	{
	}
	
	public void setModel(ErnestModel model)
	{	
		m_model = model;
	}
	
	public ArrayList<Placeable> getPlaceList()
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
		return m_model.getErnest().getClock();
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
