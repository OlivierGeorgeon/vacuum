package agent;


import java.awt.Color;

import javax.swing.JFrame;


class EyeView extends JFrame{

	
	private EyeDisplay ED;
    public EyeView(InternalView m_eye){
        
        this.setSize(720, 600);
        this.setLocationRelativeTo(null);               
        
        ED=new EyeDisplay(m_eye);
        
        this.setVisible(true);           
        this.setContentPane(ED);

    }
    
    public void setEye(InternalView eye){
    	ED.setEye(eye);
    }
    
    public void paint(double[] retine,Color[] colormap,int[] corner,
    		          double[] tactile,int[] tactileMap,int[] corner2){
    	ED.updateRetine(retine,colormap,corner,tactile,tactileMap,corner2);
    	ED.repaint();
    }
	
}
