package memory110;


import java.awt.GraphicsEnvironment ;
import java.awt.Rectangle ;

import agent.FramePlugin ;
import agent.PrintableFrame ;

public class SpaceMemoryFrame extends PrintableFrame implements FramePlugin
{
	
	public SpaceMemoryFrame() {
		this.setTitle("Spatial Memory");
		
    	this.setSize(300 * 2, 250 * 2);
    	this.setLocationRelativeTo(null);
    	Rectangle screen = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
    	this.setLocation( screen.width - 600 , screen.height - 500 );
    	this.setVisible(true);
    	
    	panel=new SpaceMemoryPanel();

    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem)
	{
		((SpaceMemoryPanel) this.panel).setMemory(mem);
	}

	@Override
	public void setDelayMove( int millis ) {
		((SpaceMemoryPanel) this.panel).setDelayMove( millis );
	}
	
	@Override
	public void anim( float angleRotation , float xTranslation ) {
		((SpaceMemoryPanel) this.panel).anim( angleRotation , xTranslation );
	}	
}