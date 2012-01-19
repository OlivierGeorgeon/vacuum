package agent;


import java.awt.Color;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.vecmath.Vector3f;


class EyeView extends JFrame{

	
	private EyeDisplay ED;
    public EyeView(InternalView m_eye){
		this.setTitle("Visual Features");       
        this.setSize(EyeDisplay.RADIUS_SCALE * 2, EyeDisplay.RADIUS_SCALE * 2);
        this.setLocationRelativeTo(null);               
        
        ED=new EyeDisplay(m_eye);
        
        this.setVisible(true);           
        this.setContentPane(ED);

    }
    
    public void setEye(InternalView eye){
    	ED.setEye(eye);
    }
    
    public void paint(){
    	ED.repaint();
    }
	
}
