import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class TactileMapPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	TactileMap tmap;
	public int width;
	
	
	public TactileMapPanel(TactileMap t){
		tmap=t;
		width=200/(40-10);//tmap.mapSize;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 800, 800);
		
		/*
		// draw the tactile retina
		width=600/(tmap.resolution*tmap.sensorRes);
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.fillRect(width*i, 110-(int)(tmap.m_tactilePressure[i]*100), width, (int)(tmap.m_tactilePressure[i]*100) );
		}
		*/
		
		

		
		
		int offsetX,offsetY;
		
		
		for (int k=0;k<Math.min(3, tmap.flowX1.size());k++){
			
			for (int i=0;i<50;i+=2){
				for (int j=0;j<50;j+=2){

					
					// draw improved flow
					offsetX=50;
					offsetY=0;
					if (k==0){
						offsetX=550;
						offsetY=550;
					}
					else if (k==1){
						offsetX=200;
						offsetY=550;
					}
					else if (k==2){
						offsetX=550;
						offsetY=150;
					}
					
					
					if (tmap.testMap[i][j]){
						g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
					
						g.drawLine(offsetX-tmap.mapSize/2*width + i*width+width/2,
								   offsetY-tmap.mapSize/2*width + j*width+width/2,
								   offsetX-tmap.mapSize/2*width + i*width+width/2+(int)(1000*tmap.flowX2.get(k)[i][j]),
								   offsetY-tmap.mapSize/2*width + j*width+width/2+(int)(1000*tmap.flowY2.get(k)[i][j]) );
					}
					else{
						g.setColor(Color.blue);
						g.drawLine(offsetX-tmap.mapSize/2*width + i*width+width/2,
								   offsetY-tmap.mapSize/2*width + j*width+width/2,
								   offsetX-tmap.mapSize/2*width + i*width+width/2+(int)(1000*tmap.flowX2.get(k)[i][j]),
								   offsetY-tmap.mapSize/2*width + j*width+width/2+(int)(1000*tmap.flowY2.get(k)[i][j]) );
					}
				}
			}
		}
		
		
		// draw sensor neuron activity in the internal map
		g.setColor(Color.black);
		g.drawRect(5, 50, 290, 195);		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.setColor(tmap.m_tactileObject[i]);
			g.fillOval(150+(int)(tmap.sensorX[i]-25*tmap.m_tactilePressure[i]), 150+(int)(tmap.sensorY[i]-25*tmap.m_tactilePressure[i]),
					   (int)(50*tmap.m_tactilePressure[i]), (int)(50*tmap.m_tactilePressure[i]));
		}

		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			// draw neurons in neuron display
			if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactileVariations[i]== 20) g.setColor(Color.green);
			else if (tmap.m_tactileVariations[i]==-20) g.setColor(Color.blue );
			else g.setColor(Color.black);
			
			// neurons display
			g.fillOval(150+(int)(tmap.sensorX[i])-2, 150+(int)(tmap.sensorY[i])-2, 4, 4);
			
			// connections of first neuron layer
			g.setColor(Color.black);
			if (i< tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 150+(int)tmap.sensorY[i  ],
					                             150+(int)tmap.sensorX[i+1], 150+(int)tmap.sensorY[i+1]);
			if (i==tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 150+(int)tmap.sensorY[i  ],
                                                 150+(int)tmap.sensorX[0  ], 150+(int)tmap.sensorY[0  ]);
			
			// connections of second neuron layer
			g.setColor(Color.LIGHT_GRAY);
			if (i>=tmap.resolution && i<2*tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 150+(int)tmap.sensorY[i  ],
                    													150+(int)tmap.sensorX[i+1], 150+(int)tmap.sensorY[i+1]);
			if (i==2*tmap.resolution-1) g.drawLine(150+(int)tmap.sensorX[i  ], 150+(int)tmap.sensorY[i  ],
												   150+(int)tmap.sensorX[tmap.resolution], 150+(int)tmap.sensorY[tmap.resolution]);
			
			// connections between first and second layer
			g.setColor(Color.black);
			if (i<tmap.resolution && tmap.sensorRes>1) g.drawLine(150+(int)tmap.sensorX[i  ], 150+(int)tmap.sensorY[i  ],
                                                150+(int)tmap.sensorX[i+tmap.resolution], 150+(int)tmap.sensorY[i+tmap.resolution]);
			
			// connections between second and third layer
			if (i>=tmap.resolution && i<2*tmap.resolution && tmap.sensorRes>2){
				    g.drawLine(150+(int)tmap.sensorX[i], 150+(int)tmap.sensorY[i],
                    150+(int)tmap.sensorX[i+tmap.resolution], 150+(int)tmap.sensorY[i+tmap.resolution]);
			}
		}
		
		/*
		// neurons in constraint display
		g.setColor(Color.black);
		int res1,res2;
		int sensRes1,sensRes2;
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			sensRes1= (int)(i/tmap.resolution);
			res1=     i-sensRes1*tmap.resolution;
			g.fillOval((int)(150-(60+sensRes1*30)*Math.sin(360/tmap.resolution*res1*Math.PI/180)),
					   (int)(450+(60+sensRes1*30)*Math.cos(360/tmap.resolution*res1*Math.PI/180)),
					   4, 4);
		}

		// constraint connections
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			sensRes1= (int)(i/tmap.resolution);
			res1=     i-sensRes1*tmap.resolution;
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				sensRes2= (int)(j/tmap.resolution);
				res2=     j-sensRes2*tmap.resolution;
				if (i!=j && tmap.connections[i][j]>0.5){
					if (tmap.m_constraints[i][j]>0) g.setColor(Color.red);
					else if (tmap.m_constraints[i][j]<0) g.setColor(Color.blue);
					else g.setColor(Color.black);
					g.drawLine((int)(150-(60+sensRes1*30)*Math.sin(360/tmap.resolution*res1*Math.PI/180)),
							   (int)(450+(60+sensRes1*30)*Math.cos(360/tmap.resolution*res1*Math.PI/180)),
							   (int)(150-(60+sensRes2*30)*Math.sin(360/tmap.resolution*res2*Math.PI/180)),
							   (int)(450+(60+sensRes2*30)*Math.cos(360/tmap.resolution*res2*Math.PI/180)));
				}
			}
		}
		*/
		
		/*
		// draw flow lines
		g.setColor(Color.black);
		for (int k=0;k<Math.min(3, tmap.flowX1.size());k++){
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
			
			// real flow line
			for (int i=5;i<45;i+=2){
				for (int j=5;j<45;j+=2){
					if (tmap.flowLineX1.get(k)[i][j][0]!=0 || tmap.flowLineY1.get(k)[i][j][0]!=0){
						g.setColor(Color.black);
						int n=0;
						boolean test=true;
						while (n<tmap.flowLength-1 && test){
							g.drawLine(offsetX-tmap.mapSize/2*width + (int)(tmap.flowLineX1.get(k)[i][j][n]*width+width/2),
									   offsetY-tmap.mapSize/2*width + (int)(tmap.flowLineY1.get(k)[i][j][n]*width+width/2),
									   offsetX-tmap.mapSize/2*width + (int)(tmap.flowLineX1.get(k)[i][j][n+1]*width+width/2),
									   offsetY-tmap.mapSize/2*width + (int)(tmap.flowLineY1.get(k)[i][j][n+1]*width+width/2) );
							
							n++;
							if (n<tmap.flowLength-1)
								if (tmap.flowLineX1.get(k)[i][j][n+1]==0 && tmap.flowLineY1.get(k)[i][j][n+1]==0) test=false;
						}
					}
					
					// extrapolated flow lines
					else if (tmap.flowLineX2.get(k)[i][j][0]!=0 || tmap.flowLineY2.get(k)[i][j][0]!=0){
						g.setColor(Color.blue);
						int n=0;
						boolean test=true;
						while (n<tmap.flowLength-1 && test){
							g.drawLine(offsetX-tmap.mapSize/2*width + (int)(tmap.flowLineX2.get(k)[i][j][n]*width+width/2),
									   offsetY-tmap.mapSize/2*width + (int)(tmap.flowLineY2.get(k)[i][j][n]*width+width/2),
									   offsetX-tmap.mapSize/2*width + (int)(tmap.flowLineX2.get(k)[i][j][n+1]*width+width/2),
									   offsetY-tmap.mapSize/2*width + (int)(tmap.flowLineY2.get(k)[i][j][n+1]*width+width/2) );
							
							n++;
							if (tmap.flowLineX2.get(k)[i][j][n+1]==0 && tmap.flowLineY2.get(k)[i][j][n+1]==0) test=false;
						}
					}

				}
			}
		}*/

	}
	
}

