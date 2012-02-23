package agent;
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

/**
 * Panel used to display several information on the tactile colliculus
 * @author simon
 */
public class InnerEarPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	InnerEar imap;
	
	
	public InnerEarPanel(InnerEar t){
		imap=t;
	}
	
	public void setInnerEar(InnerEar t){
		imap=t;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 400, 200);
		
		g.setColor(Color.black);
		g.fillRect(199,0,2,200);
		
		g.drawLine(5, 100, 195, 100);
		g.drawLine(205, 100, 395, 100);
		
		g.drawLine(100, 5, 100, 195);
		g.drawLine(300, 5, 300, 195);
		
		
		g.setColor(Color.red);
		
		int coef=500;
		g.drawLine(100,100,100-(int)(imap.left_y*coef),100-(int)(imap.left_x*coef) );
		g.drawLine(300,100,300-(int)(imap.right_y*coef),100-(int)(imap.right_x*coef) );
		
		g.fillOval(100-(int)(imap.left_y*coef)-2, 100-(int)(imap.left_x*coef)-2, 4, 4);
		g.fillOval(300-(int)(imap.right_y*coef)-2, 100-(int)(imap.right_x*coef)-2, 4, 4);
		
		
		g.setColor(Color.white);
		g.fillRect(0, 250, 400, 200);
		
		g.setColor(Color.red);
		for (int i=0;i<200;i++){
			for (int j=0;j<200;j++){
				if (imap.leftMap[i][j] >0) g.fillRect(200-j, 450-i, 1, 1);
				if (imap.rightMap[i][j]>0) g.fillRect(400-j, 450-i, 1, 1);
			}
		}
	}
	
}

