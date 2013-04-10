package memory110;


import java.awt.GraphicsEnvironment ;
import java.awt.Rectangle ;

import javax.swing.JPanel;

import agent.Main ;
import agent.PrintableFrame;
import agent.PrintablePanel;

public class SpaceMemoryFrame extends PrintableFrame
{
	
	public SpaceMemoryFrame(SpaceMemory spaceMemory)
	{
		this.setTitle("Spatial Memory");
		
    	this.setSize(300 * 2, 250 * 2);
    	this.setLocationRelativeTo(null);
    	Rectangle screen = GraphicsEnvironment.getLocalGraphicsEnvironment().getMaximumWindowBounds();
    	this.setLocation( screen.width - 600 , screen.height - 500 );
    	this.setVisible(true);

    	panel=new SpaceMemoryPanel(spaceMemory);
    	
    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem)
	{
		((SpaceMemoryPanel) panel).setMemory(mem);
	}	
}