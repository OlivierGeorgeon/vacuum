package paused;
import java.util.ArrayList;

import javax.swing.JFrame;


public class PatternMappingFrame extends JFrame{

	private PatternMap patternMap;
	private PatternMappingPanel panel;
	
	public PatternMappingFrame(PatternMap p){
		this.setTitle("patern Map");
    	this.setSize(1400, 700);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	patternMap=p;
    	panel=new PatternMappingPanel(p);
    	
    	this.setContentPane(panel);
	}
	
	public void update(int x2,int y2,int angle){
		panel.x=x2;
		panel.y=y2;
		panel.theta=angle;
		panel.setMap();
	}
	
	public void paint(){
    	panel.repaint();
    }
}
