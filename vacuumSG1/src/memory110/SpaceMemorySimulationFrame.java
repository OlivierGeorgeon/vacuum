package memory110;


import javax.swing.JPanel;
import agent.PrintableFrame;
import agent.PrintablePanel;

public class SpaceMemorySimulationFrame extends PrintableFrame
{
	
	public SpaceMemorySimulationFrame(SpaceMemory spaceMemory)
	{
		this.setTitle("Spatial simulation");
		
    	this.setSize(300 * 2, 250 * 2);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new SpaceMemorySimulationPanel(spaceMemory);
    	
    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem)
	{
		((SpaceMemorySimulationPanel) panel).setMemory(mem);
	}	
}