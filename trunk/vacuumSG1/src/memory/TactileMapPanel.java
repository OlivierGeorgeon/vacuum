package memory;
import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

/**
 * Panel used to display several information on the tactile colliculus
 * @author simon
 */
public class TactileMapPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	TactileMap tmap;
	public int width,width1,width2;
	
	
	public TactileMapPanel(TactileMap t){
		tmap=t;
		width=200/(40-10);//tmap.mapSize;
		
		width1=800/(3*12+1);
	}
	
	public void setTactile(TactileMap t){
		tmap=t;
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
			
			// draw neuron flow vector
			for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			
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
				
				g.setColor(Color.blue);
				g.drawLine(offsetX+(int)(tmap.sensorX[i]), offsetY+(int)(tmap.sensorY[i]),
						   offsetX+(int)(tmap.sensorX[i]+1000*tmap.flowVectorX.get(k)[i]),
						   offsetY+(int)(tmap.sensorY[i]+1000*tmap.flowVectorY.get(k)[i]) );
			}
			
			/*
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
					
					
					if (tmap.potentialTestMap[i][j]){
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
			} /**/
		}
		
		/*
		// draw sensor neuron activity in the internal map
		g.setColor(Color.black);
		g.drawRect(5, 30, 290, 230);		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.setColor(Color.black);
			if (tmap.m_tactileObject[i%tmap.resolution]==1) g.setColor(Color.gray);
			if (tmap.m_tactileObject[i%tmap.resolution]==2) g.setColor(Color.lightGray);
			if (tmap.m_tactileObject[i%tmap.resolution]==3) g.setColor(Color.white);
			g.fillOval(150+(int)(tmap.sensorX[i]-25*tmap.m_tactilePressure[i]), 150+(int)(tmap.sensorY[i]-25*tmap.m_tactilePressure[i]),
					   (int)(50*tmap.m_tactilePressure[i]), (int)(50*tmap.m_tactilePressure[i]));
		}*/

		
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			// draw neurons in neuron display
			/*if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactilePressure[i]== 1) g.setColor(Color.green);
			else if (tmap.m_tactilePressure[i]== 2) g.setColor(Color.blue );
			else if (tmap.m_tactilePressure[i]== 3) g.setColor(Color.green );
			else 
				g.setColor(Color.black);*/
			
			g.setColor(new Color((int)tmap.timerMap[i]*5,(40-(int)tmap.timerMap[i])*5,0));
			
			// neurons display
			g.fillOval(150+(int)(tmap.sensorX[i]*6)-2, 150+(int)(tmap.sensorY[i]*6)-2, 4, 4);
			
			// connections of first neuron layer
			g.setColor(Color.black);
			if (i< tmap.resolution-1) g.drawLine(150+(int)(tmap.sensorX[i  ]*6), 150+(int)(tmap.sensorY[i  ]*6),
					                             150+(int)(tmap.sensorX[i+1]*6), 150+(int)(tmap.sensorY[i+1]*6));
			if (i==tmap.resolution-1) g.drawLine(150+(int)(tmap.sensorX[i  ]*6), 150+(int)(tmap.sensorY[i  ]*6),
                                                 150+(int)(tmap.sensorX[0  ]*6), 150+(int)(tmap.sensorY[0  ]*6));
			
			// connections of second neuron layer
			g.setColor(Color.LIGHT_GRAY);
			if (i>=tmap.resolution && i<2*tmap.resolution-1) g.drawLine(150+(int)(tmap.sensorX[i  ]*6), 150+(int)(tmap.sensorY[i  ]*6),
                    													150+(int)(tmap.sensorX[i+1]*6), 150+(int)(tmap.sensorY[i+1]*6));
			if (i==2*tmap.resolution-1) g.drawLine(150+(int)(tmap.sensorX[i  ]*6), 150+(int)(tmap.sensorY[i  ]*6),
												   150+(int)(tmap.sensorX[tmap.resolution]*6), 150+(int)(tmap.sensorY[tmap.resolution]*6));
			
			// connections between first and second layer
			g.setColor(Color.black);
			if (i<tmap.resolution && tmap.sensorRes>1) g.drawLine(150+(int)(tmap.sensorX[i  ]*6), 150+(int)(tmap.sensorY[i  ]*6),
                                                150+(int)(tmap.sensorX[i+tmap.resolution]*6), 150+(int)(tmap.sensorY[i+tmap.resolution]*6));
			
			// connections between second and third layer
			if (i>=tmap.resolution && i<2*tmap.resolution && tmap.sensorRes>2){
				    g.drawLine(150+(int)(tmap.sensorX[i]*6), 150+(int)(tmap.sensorY[i]*6),
                    150+(int)(tmap.sensorX[i+tmap.resolution]*6), 150+(int)(tmap.sensorY[i+tmap.resolution]*6));
			}
			
			
			// speed direction
			g.setColor(Color.red);
			if (tmap.speedDirectionX.size()>0){
				g.drawLine(450+(int)(tmap.sensorX[i]*6), 150+(int)(tmap.sensorY[i]*6),
						   450+(int)(tmap.sensorX[i]*6)+ 20*(int)tmap.speedDirectionX.get(0)[i],
						   150+(int)(tmap.sensorY[i]*6)+ 20*(int)tmap.speedDirectionY.get(0)[i]);
			}
			
			if (tmap.speedDirectionX.size()>1){
				g.drawLine(150+(int)(tmap.sensorX[i]*6), 450+(int)(tmap.sensorY[i]*6),
						   150+(int)(tmap.sensorX[i]*6)+ 20*(int)tmap.speedDirectionX.get(1)[i],
						   450+(int)(tmap.sensorY[i]*6)+ 20*(int)tmap.speedDirectionY.get(1)[i]);
			}
			
			if (tmap.speedDirectionX.size()>2){
				g.drawLine(450+(int)(tmap.sensorX[i]*6), 450+(int)(tmap.sensorY[i]*6),
						   450+(int)(tmap.sensorX[i]*6)+ 20*(int)tmap.speedDirectionX.get(2)[i],
						   450+(int)(tmap.sensorY[i]*6)+ 20*(int)tmap.speedDirectionY.get(2)[i]);
			}/* */
			
			/*
			// movement relation
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				if (tmap.speedDirectionX.size()>0){
					g.setColor(new Color(0,0,tmap.speedDirection.get(0)[i][j]));
					g.fillRect(450+i*5, 10+j*5, 5, 5);
				}
			
				if (tmap.speedDirectionX.size()>1){
					g.setColor(new Color(0,0,tmap.speedDirection.get(1)[i][j]));
					g.fillRect(20+i*5, 450+j*5, 5, 5);
				}
			
				if (tmap.speedDirectionX.size()>2){
					g.setColor(new Color(0,0,tmap.speedDirection.get(1)[i][j]));
					g.fillRect(450+i*5, 450+j*5, 5, 5);
				}
			}/* */
			
			/*
			// relative movement relation
			for (int j=0;j<i;j++){
				if (tmap.speedDirectionX.size()>0){
					g.setColor(new Color(0,0,0.5f+(tmap.speedDirection.get(0)[i][j]-tmap.speedDirection.get(0)[j][i])/2));
					g.fillRect(450+i*5, 10+j*5, 5, 5);
				}
			
				if (tmap.speedDirectionX.size()>1){
					g.setColor(new Color(0,0,0.5f+(tmap.speedDirection.get(1)[i][j]-tmap.speedDirection.get(1)[j][i])/2));
					g.fillRect(20+i*5, 450+j*5, 5, 5);
				}
			
				if (tmap.speedDirectionX.size()>2){
					g.setColor(new Color(0,0,0.5f+(tmap.speedDirection.get(2)[i][j]-tmap.speedDirection.get(2)[j][i])/2));
					g.fillRect(450+i*5, 450+j*5, 5, 5);
				}
			}/* */
		}
		
		
		
		//draw neuron distances
		/*
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			//for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
			//	g.fillRect(i*width1,j*width1,width1-1,(int)((width1-1)*tmap.connections[i][j]/20));
			//}
			
			g.setColor(Color.blue);
			g.fillRect(i*width1          ,0,width1/2-1,(int)(800*(500-tmap.connections[i][18])/(500)));
			g.setColor(Color.green);
			g.fillRect(i*width1+width/2+1,0,width1/2-1,(int)(10*Math.sqrt( (tmap.sensorX[i]-tmap.sensorX[18])*(tmap.sensorX[i]-tmap.sensorX[18])
															      	  +(tmap.sensorY[i]-tmap.sensorY[18])*(tmap.sensorY[i]-tmap.sensorY[18]))) );
		}*/
		
		
		
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

