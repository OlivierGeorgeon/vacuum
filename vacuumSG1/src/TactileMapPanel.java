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
		
		// draw the tactile retina
		int width=600/tmap.resolution;
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution;i++){
			g.fillRect(width*i, 110-(int)(tmap.m_tactilePressure[i]*100), width, (int)(tmap.m_tactilePressure[i]*100) );
		}
		
		
		
		// draw the potential map
		g.setColor(Color.black);
		g.drawRect(5, 345, 290, 210);
		
		width=200/tmap.mapSize;
		for (int i=0;i<tmap.mapSize;i++){
			for (int j=0;j<tmap.mapSize;j++){
				g.setColor(new Color(tmap.chargeMap[i][j],1-tmap.chargeMap[i][j],0));
				g.fillRect(150-tmap.mapSize/2*width + i*width, 450-tmap.mapSize/2*width + j*width, width, width);
			}
		}
		
		
		// draw sensor neurons in the internal map
		g.setColor(Color.black);
		g.drawRect(5, 150, 290, 195);		
		for (int i=0;i<tmap.resolution;i++){
			g.setColor(tmap.m_tactileObject[i]);
			g.fillOval(150+(int)(tmap.sensorX[i]-25*tmap.m_tactilePressure[i]), 250+(int)(tmap.sensorY[i]-25*tmap.m_tactilePressure[i]),
					   (int)(50*tmap.m_tactilePressure[i]), (int)(50*tmap.m_tactilePressure[i]));
		}
		
		for (int i=0;i<tmap.resolution;i++){
			// draw neurons in neuron display
			if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactileVariations[i]== 20) g.setColor(Color.green);
			else if (tmap.m_tactileVariations[i]==-20) g.setColor(Color.blue );
			else g.setColor(Color.black);
			g.fillOval(150+(int)(tmap.sensorX[i])-2, 250+(int)(tmap.sensorY[i])-2, 4, 4);
			g.setColor(Color.black);
			if (i<tmap.resolution-1)g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
					                           150+(int)tmap.sensorX[i+1], 250+(int)tmap.sensorY[i+1]);
			
			// draw neurons in potential display
			g.fillOval(150+(int)(tmap.sensorX[i])/2-1, 450+(int)(tmap.sensorY[i])/2-1, 2, 2);
		}
	}
	
}

