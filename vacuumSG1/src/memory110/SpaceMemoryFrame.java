package memory110;

import java.util.ArrayList;
import java.util.List;

import javax.swing.JFrame;

import spas.IPlace;

import memory.TactileMap;
import memory.TactileMapPanel;


public class SpaceMemoryFrame extends JFrame{

	private SpaceMemoryPanel panel;
	
	public SpaceMemoryFrame(){
		this.setTitle("space memory");
    	this.setSize(500, 500);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new SpaceMemoryPanel();
    	
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