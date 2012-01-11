package memory110;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;

import spas.IPlace;

import memory.TactileMap;
import memory.TactileMapPanel;


public class SpaceMemoryFrame extends JFrame{

	private SpaceMemoryPanel panel;
	
	public SpaceMemoryFrame(SpaceMemory spaceMemory){
		this.setTitle("Local Space Memory");
		int side = 2 * SpaceMemoryPanel.RADIUS * SpaceMemoryPanel.SCALE;
    	this.setSize(side, side);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new SpaceMemoryPanel(spaceMemory);
    	
    	this.setContentPane(panel);
	}
	
	public void setMemory(SpaceMemory mem){
		panel.setMemory(mem);
	}
	
	public void update(ArrayList<IPlace> list){
		panel.update(list);
	}
	
	public void paint(){
    	panel.repaint();
    }
}