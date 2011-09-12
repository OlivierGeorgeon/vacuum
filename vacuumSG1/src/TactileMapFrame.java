import java.util.ArrayList;

import javax.swing.JFrame;


public class TactileMapFrame extends JFrame{

	private TactileMap tactileMap;
	private TactileMapPanel panel;
	
	public TactileMapFrame(TactileMap t){
		this.setTitle("tactile Map");
    	this.setSize(600, 600);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	tactileMap=t;
    	panel=new TactileMapPanel(t);
    	
    	this.setContentPane(panel);
	}
	
	
	public void paint(){
    	panel.repaint();
    }
}
