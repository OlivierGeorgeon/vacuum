package memory110;


import javax.swing.JPanel;
import agent.PrintableFrame;
import agent.PrintablePanel;

public class SpaceMemoryFrame extends PrintableFrame
{
	
	public SpaceMemoryFrame(SpaceMemory spaceMemory)
	{
		this.setTitle("Spatial Memory");
		
    	this.setSize(300 * 2, 250 * 2);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new SpaceMemoryPanel(spaceMemory);
    	
    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem)
	{
		((SpaceMemoryPanel) panel).setMemory(mem);
	}	
}