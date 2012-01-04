package paused;
import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;
import java.util.ArrayList;

import javax.imageio.ImageIO;
import javax.swing.JFrame;



public class InternalStatesFrame extends JFrame{

	private InternalStatesPanel panel;
	private int indexImage;
	
	public InternalStatesFrame(ArrayList<Action> actList){
		this.setTitle("Internal state");
    	this.setSize(1200, 620);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);
    	
    	indexImage=0;
    	
    	panel=new InternalStatesPanel(actList);
    	
    	this.setContentPane(panel);
	}
	
	
	public void saveImage(){
		
		String path="/home/simon/Bureau/Ernest/";
		
		if (indexImage<10) path+="000"+indexImage+".jpg";
		else if (indexImage<100) path+="00"+indexImage+".jpg";
		else                     path+="0" +indexImage+".jpg";
		
		 BufferedImage image = new BufferedImage(this.getWidth(),
                this.getHeight(),
                BufferedImage.TYPE_INT_RGB);
		 		Graphics2D g2 = image.createGraphics();
		 		this.paint(g2);
		 		g2.dispose();
		 		try {
		 			ImageIO.write(image, "JPEG", new File(path));
		 		} catch (Exception e) { }
		 		indexImage++;
	}
	
	
	public void paint(){
    	panel.repaint();
    }
	
}
