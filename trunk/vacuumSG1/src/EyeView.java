

import java.awt.Color;

import javax.swing.JFrame;


class EyeView extends JFrame{

	
	private EyeDisplay ED;
    public EyeView(InternalMap m){
        
        this.setSize(720, 300);
        this.setLocationRelativeTo(null);               
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        
        ED=new EyeDisplay(m);
        
        this.setVisible(true);           
        this.setContentPane(ED);

    }
    
    public void paint(double[] retine,Color[] colormap,int[] corner,
    		          double[] tactile,int[] tactileMap,int[] corner2){
    	ED.updateRetine(retine,colormap,corner,tactile,tactileMap,corner2);
    	ED.repaint();
    }
	
}
