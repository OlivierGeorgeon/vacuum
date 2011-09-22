import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class TactileMapPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	TactileMap tmap;
	
	
	public TactileMapPanel(TactileMap t){
		tmap=t;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 600, 600);

		
		// draw the tactile retina
		int width=600/(tmap.resolution*tmap.sensorRes);
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.fillRect(width*i, 110-(int)(tmap.m_tactilePressure[i]*100), width, (int)(tmap.m_tactilePressure[i]*100) );
		}
		
		
		
		// draw the potential map
		g.setColor(Color.black);
		g.drawRect(5, 345, 290, 210);
		
		width=200/(40-10);//tmap.mapSize;
		int offsetX,offsetY;
		for (int k=0;k<Math.min(3, tmap.flowX1.size());k++){
			for (int i=10;i<40;i++){
				for (int j=10;j<40;j++){
				
					// potential map
					//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
					//g.fillRect(450-tmap.mapSize/2*width + i*width, 450-tmap.mapSize/2*width + j*width, width, width);

					// draw flow
					g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
				
				
					// draw improved flow
					offsetX=0;
					offsetY=0;
					if (k==0){
						offsetX=450;
						offsetY=450;
					}
					else if (k==1){
						offsetX=150;
						offsetY=450;
					}
					else if (k==2){
						offsetX=450;
						offsetY=250;
					}
					
					if (tmap.flowX1.size()>0 && tmap.flowX1.get(k)[i][j]!=0 || tmap.flowY1.get(k)[i][j]!=0)
						g.drawLine(offsetX-tmap.mapSize/2*width + i*width+width/2,offsetY-tmap.mapSize/2*width + j*width+width/2,
								   offsetX-tmap.mapSize/2*width + i*width+width/2+(int)(10*tmap.flowX2.get(k)[i][j]),
								   offsetY-tmap.mapSize/2*width + j*width+width/2+(int)(10*tmap.flowY2.get(k)[i][j]) );
				
				}
			}
		}
		
		
		// draw sensor neurons in the internal map
		g.setColor(Color.black);
		g.drawRect(5, 150, 290, 195);		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.setColor(tmap.m_tactileObject[i]);
			g.fillOval(150+(int)(tmap.sensorX[i]-25*tmap.m_tactilePressure[i]), 250+(int)(tmap.sensorY[i]-25*tmap.m_tactilePressure[i]),
					   (int)(50*tmap.m_tactilePressure[i]), (int)(50*tmap.m_tactilePressure[i]));
		}
		
		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			// draw neurons in neuron display
			if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactileVariations[i]== 20) g.setColor(Color.green);
			else if (tmap.m_tactileVariations[i]==-20) g.setColor(Color.blue );
			else g.setColor(Color.black);
			
			// neurons
			g.fillOval(150+(int)(tmap.sensorX[i])-2, 250+(int)(tmap.sensorY[i])-2, 4, 4);
			
			// connections of first neuron layer
			g.setColor(Color.black);
			if (i< tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
					                             150+(int)tmap.sensorX[i+1], 250+(int)tmap.sensorY[i+1]);
			if (i==tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
                                                 150+(int)tmap.sensorX[0  ], 250+(int)tmap.sensorY[0  ]);
			
			// connections of second neuron layer
			g.setColor(Color.LIGHT_GRAY);
			if (i>=tmap.resolution && i<2*tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
                    													150+(int)tmap.sensorX[i+1], 250+(int)tmap.sensorY[i+1]);
			if (i==2*tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
												   150+(int)tmap.sensorX[tmap.resolution], 250+(int)tmap.sensorY[tmap.resolution]);
			
			// connections between first and second layer
			g.setColor(Color.black);
			if (i<tmap.resolution && tmap.sensorRes>1) g.drawLine(150+(int)tmap.sensorX[i  ], 250+(int)tmap.sensorY[i  ],
                                                150+(int)tmap.sensorX[i+tmap.resolution], 250+(int)tmap.sensorY[i+tmap.resolution]);
			
			// connections between second and third layer
			if (i>=tmap.resolution && i<2*tmap.resolution && tmap.sensorRes>2){
				    g.drawLine(150+(int)tmap.sensorX[i], 250+(int)tmap.sensorY[i],
                    150+(int)tmap.sensorX[i+tmap.resolution], 250+(int)tmap.sensorY[i+tmap.resolution]);
			}

		}

	}
	
}

