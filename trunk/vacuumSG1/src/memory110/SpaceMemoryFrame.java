package memory110;

import javax.swing.JFrame;

public class SpaceMemoryFrame extends JFrame
{
	private SpaceMemoryPanel panel;
	
	public SpaceMemoryFrame(SpaceMemory spaceMemory)
	{
		this.setTitle("Space Memory");
		
    	this.setSize(300 * 2, 250 * 2);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new SpaceMemoryPanel(spaceMemory);
    	
    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem)
	{
		panel.setMemory(mem);
	}	
}