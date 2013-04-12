package memory110;

import java.awt.Graphics ;

import agent.PrintablePanel ;
//import agent.Ernest110Model;




public class SpaceMemoryPanel extends PrintablePanel
{
	private static final long serialVersionUID = 1L;
	
	//public int index;
	//private IErnest m_ernest;
	
	public SpaceMemory spaceMemory;

	private int delayMove ;
	private float angleRotation ;
	private float xTranslation ;
	
	public SpaceMemoryPanel(){
	}
	
	public void setMemory(SpaceMemory mem){
		spaceMemory = mem;
	}
	
	public void setDelayMove( int millis ) {
		this.delayMove = millis;
	}
	
	public void anim( float angleRotation , float xTranslation ) {
		this.angleRotation = 0;
		this.xTranslation = 0;
		for ( int i = 0; i < 20; i++ ) {
			this.angleRotation += angleRotation / 20;
			this.xTranslation += xTranslation / 20;
			this.repaint();
			try {
				Thread.currentThread().sleep( this.delayMove );
			} catch ( InterruptedException e ) {
			}
		}
	}	

	public void paintComponent(Graphics g)
	{
//		spaceMemory.m_model.paintSpaceMemory(g);   
		if( this.spaceMemory != null && this.spaceMemory.m_model != null )
			spaceMemory.m_model.paintSpaceMemory(g, spaceMemory.m_model.getErnest().getPlaceList() , this.angleRotation , this.xTranslation );
	}

	
}
