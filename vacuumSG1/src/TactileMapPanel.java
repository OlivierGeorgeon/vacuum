import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class TactileMapPanel extends JPanel{

	TactileMap tmap;
	
	
	public TactileMapPanel(TactileMap t){
		tmap=t;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 600, 600);
		
		int width=600/tmap.resolution;
		
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution;i++){
			g.fillRect(width*i, 110-(int)(tmap.m_tactilePressure[i]*100), width, (int)(tmap.m_tactilePressure[i]*100) );
		}
		
		g.drawRect(100, 150, 400, 400);		
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution;i++){
			if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactileVariations[i]!=0) g.setColor(Color.green);
			else g.setColor(Color.black);
			g.fillOval(300+(int)(tmap.sensorX[i]), 350+(int)(tmap.sensorY[i]), 5, 5);
		}
		
		/*for (int i=0;i<tmap.resolution;i++){
			for (int j=0;j<tmap.resolution;j++){
				System.out.println(tmap.connections[i][j]);
				g.setColor(new Color((float)Math.max(0,1-(tmap.connections[i][j]/300)),(float)Math.min(1,tmap.connections[i][j]/300),0));
				g.fillRect(110+i*10, 160+j*10, 10, 10);
			}
		}*/
		
	}
	
}

