package agent;


import java.awt.Color;
import java.util.ArrayList;

import javax.swing.JFrame;
import javax.vecmath.Vector3f;


class EyeView extends PrintableFrame{

	
	//private EyeDisplay ED;
    public EyeView(InternalView m_eye){
		this.setTitle("Visual Features");       
        this.setSize(EyeDisplay.WIDTH * 2, EyeDisplay.HEIGHT * 2);
        this.setLocationRelativeTo(null);               
        
        panel=new EyeDisplay(m_eye);
        
        this.setVisible(true);           
        this.setContentPane(panel);

    }
    
    public void setEye(InternalView eye){
    	((EyeDisplay) panel).setEye(eye);
    }
    
    public void paint(){
    	panel.repaint();
    }
	
}
