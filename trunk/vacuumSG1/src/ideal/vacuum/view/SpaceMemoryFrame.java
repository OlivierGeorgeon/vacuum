package ideal.vacuum.view;


import ideal.vacuum.FramePlugin ;

import java.awt.Graphics ;
import java.awt.GraphicsEnvironment ;
import java.awt.Rectangle ;

import javax.swing.JFrame ;
import javax.swing.JPanel ;


public class SpaceMemoryFrame extends JFrame implements FramePlugin{
	
	private JPanel contentPane ;
	public SpaceMemory spaceMemory;

	private int delayMove ;
	private float angleRotation ;
	private float xTranslation ;
	
	public SpaceMemoryFrame() {
		this.setTitle("Spatial Memory");
		
		this.setSize(300 * 2, 250 * 2);
		this.setLocationRelativeTo(null);
    	Rectangle screen = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
    	this.setLocation( screen.width - 600 , screen.height - 500 );
    	this.setVisible(true);
    	
//    	this.contentPane=new SpaceMemoryPanel();

    	this.setContentPane(this.contentPane);
	}

	public void setMemory(SpaceMemory mem)
	{
		this.spaceMemory = mem;
	}

	@Override
	public void setDelayMove( int millis ) {
		this.delayMove = millis;
	}
	
	@Override
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
	
	@Override
	public void paintComponents( Graphics g ) {
		super.paintComponents( g ) ;
		if( this.spaceMemory != null && this.spaceMemory.m_model != null )
			spaceMemory.m_model.paintSpaceMemory(g, spaceMemory.m_model.getErnest().getPlaceList() , this.angleRotation , this.xTranslation );
	}
}