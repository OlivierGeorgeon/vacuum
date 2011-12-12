import java.awt.Graphics2D;
import java.awt.image.BufferedImage;
import java.io.File;

import javax.imageio.ImageIO;
import javax.swing.JFrame;


public class ColliculusFrame extends JFrame{

	private ColliculusPanel panel;
	
	private int indexImage;
	
	public ColliculusFrame(Colliculus c){
		this.setTitle("Colliculus");
    	this.setSize(1200, 800);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new ColliculusPanel(c);
    	
    	indexImage=0;
    	
    	this.setContentPane(panel);
	}
	
	public void saveImage(){
		
		String path="/home/simon/Bureau/Ernest/map/";
		
		if (indexImage<10) path+="0000"+indexImage+".jpg";
		else if (indexImage<100 ) path+="000"+indexImage+".jpg";
		else if (indexImage<1000) path+="00" +indexImage+".jpg";
		else                      path+="0"  +indexImage+".jpg";
		
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