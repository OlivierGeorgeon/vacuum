package ideal.vacuum.view;

import ideal.vacuum.ErnestModel ;

import java.util.ArrayList;

import javax.vecmath.Vector3f;


import eca.spas.egomem.ActInstance;
import ernest.IErnest;


public class SpaceMemory 
{
	public ErnestModel m_model;
	private ArrayList<ActInstance> placeList = new ArrayList<ActInstance>();
	
	public SpaceMemory()
	{
	}
	
	public void setModel(ErnestModel model)
	{	
		m_model = model;
	}
	
	public ArrayList<ActInstance> getPlaceList()
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
