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
	public int index;
	public Color[][] sensorSequence;
	
	
	public TactileMapPanel(TactileMap t){
		tmap=t;
		width=200/(40-10);//tmap.mapSize;
		
		width1=800/(3*12+1);
		
		index=0;
		sensorSequence=new Color[tmap.resolution*tmap.sensorRes][1000];
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			for (int j=0;j<1000;j++){
				sensorSequence[i][j]=Color.white;
			}
		}
	}
	
	public void setTactile(TactileMap t){
		tmap=t;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 300, 800);
		

		int offsetX,offsetY;

		offsetX=160;
		offsetY=160;
		// draw "neuron" network
		int size=5;
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			// draw neurons in neuron display
			/*if (i==tmap.resolution/2) g.setColor(Color.red);
			else if (tmap.m_tactilePressure[i]== 1) g.setColor(Color.green);
			else if (tmap.m_tactilePressure[i]== 2) g.setColor(Color.blue );
			else if (tmap.m_tactilePressure[i]== 3) g.setColor(Color.green );
			else 
				g.setColor(Color.black);*/
			
			
			/*
			// action link
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				if (i!=j && (tmap.actionLink[i][j][0]>0.5 || tmap.actionLink[i][j][1]>0.5 || tmap.actionLink[i][j][2]>0.5)){
					g.setColor(new Color(tmap.actionLink[i][j][0],tmap.actionLink[i][j][1],tmap.actionLink[i][j][2]));
					//g.fillRect(10+i*8, 320+j*8, 8, 8);
					double dx=tmap.sensorX[i]-tmap.sensorX[j];
					double dy=tmap.sensorY[i]-tmap.sensorY[j];
					
					double dx2=dx/(Math.sqrt(dx*dx+dy*dy))*2;
					double dy2=dy/(Math.sqrt(dx*dx+dy*dy))*2;
					
					g.drawLine(offsetX+(int)(tmap.sensorX[i]*size), offsetX+(int)(tmap.sensorY[i]*size),
				  		       offsetX+(int)((tmap.sensorX[i]-dx2)*size), offsetX+(int)((tmap.sensorY[i]-dy2)*size));
				}
			}/**/
			
			//g.setColor(new Color((int)tmap.timerMap[i]*5,(40-(int)tmap.timerMap[i])*5,0));
			
			//g.setColor(new Color((int)(tmap.m_tactileVariations[i]*5), 250-(int)(tmap.m_tactileVariations[i]*5),0));
			
			if (tmap.m_tactilePressure[i]!=0) g.setColor(Color.green);
			else                              g.setColor(Color.red);
			
			// neurons display
			g.fillOval(offsetX+(int)(tmap.sensorX[i]*size)-size/2, offsetX+(int)(tmap.sensorY[i]*size)-size/2, size, size);
			//g.fillOval(offsetX+(int)(tmap.sensorX[i]*size)-2, offsetX+300+(int)(tmap.sensorY[i]*size)-2, 4, 4);
			
			// connections of first neuron layer
			g.setColor(Color.black);
			if (i< tmap.resolution-1) g.drawLine(offsetX+(int)(tmap.sensorX[i  ]*size), offsetX+(int)(tmap.sensorY[i  ]*size),
					offsetX+(int)(tmap.sensorX[i+1]*size), offsetX+(int)(tmap.sensorY[i+1]*size));
			if (i==tmap.resolution-1) g.drawLine(offsetX+(int)(tmap.sensorX[i  ]*size), offsetX+(int)(tmap.sensorY[i  ]*size),
					offsetX+(int)(tmap.sensorX[0  ]*size), offsetX+(int)(tmap.sensorY[0  ]*size));
			
			// connections of second neuron layer
			g.setColor(Color.LIGHT_GRAY);
			if (i>=tmap.resolution && i<2*tmap.resolution-1) g.drawLine(offsetX+(int)(tmap.sensorX[i  ]*size), offsetX+(int)(tmap.sensorY[i  ]*size),
                    													offsetX+(int)(tmap.sensorX[i+1]*size), offsetX+(int)(tmap.sensorY[i+1]*size));
			if (i==2*tmap.resolution-1) g.drawLine(offsetX+(int)(tmap.sensorX[i  ]*size), offsetX+(int)(tmap.sensorY[i  ]*size),
												   offsetX+(int)(tmap.sensorX[tmap.resolution]*size), offsetX+(int)(tmap.sensorY[tmap.resolution]*size));
			
			// connections between first and second layer
			g.setColor(Color.black);
			if (i<tmap.resolution && tmap.sensorRes>1) g.drawLine(offsetX+(int)(tmap.sensorX[i  ]*size), offsetX+(int)(tmap.sensorY[i  ]*size),
                                                offsetX+(int)(tmap.sensorX[i+tmap.resolution]*size), offsetX+(int)(tmap.sensorY[i+tmap.resolution]*size));
			
			// connections between second and third layer
			if (i>=tmap.resolution && i<2*tmap.resolution && tmap.sensorRes>2){
				    g.drawLine(offsetX+(int)(tmap.sensorX[i]*size), offsetX+(int)(tmap.sensorY[i]*size),
                    offsetX+(int)(tmap.sensorX[i+tmap.resolution]*size), offsetX+(int)(tmap.sensorY[i+tmap.resolution]*size));
			}/**/

		}
		
		/*
		// draw neutral neuron network
		g.setColor(Color.orange);
		for (int i=0;i<20;i++){
			g.fillOval(offsetX+(int)(tmap.neutralX[i]*size)-2, offsetX+300+(int)(tmap.neutralY[i]*size)-2, 4, 4);
		}/**/
		
		/*
		// draw voronoi matrix
		int red,green,blue;
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				if (tmap.voronoiPoints[i][j]==-1){
					g.setColor(Color.black);
				}
				else{
					if (tmap.voronoiPoints[i][j]<tmap.resolution*tmap.sensorRes){
						if (tmap.m_tactilePressure[tmap.voronoiPoints[i][j]]==0){
							g.setColor(Color.green);
						}
						else{
							g.setColor(Color.red);
						}
					}
					else{
						g.setColor(Color.orange);
					}
				}
				
				g.fillRect(500+2*i, 50+2*j, 2, 2);
				
				
				if (tmap.voronoiPoints[i][j]==-1){
					g.setColor(Color.black);
				}
				else{
					red= (tmap.voronoiPoints[i][j]%3)*120;
					green=(tmap.voronoiPoints[i][j]%4)*60;
					blue=(tmap.voronoiPoints[i][j]%5)*50;
					g.setColor(new Color(red,green,blue));
				
					g.fillRect(500+2*i, 300+2*j, 2, 2);
				}
			}
		}
		
		g.setColor(Color.blue);
		g.fillOval(600 +(int)(4*tmap.Gx), 150 +(int)(4*tmap.Gy), 5, 5);
		
		// draw delaunay links
		int x1,y1,x2,y2;
		g.setColor(Color.red);
		for (int i=0;i<tmap.resolution*tmap.sensorRes+20;i++){
			for (int j=0;j<tmap.resolution*tmap.sensorRes+20;j++){
				
				if (tmap.delaunayLinks[i][j]){
				
					if (i<tmap.resolution*tmap.sensorRes) x1=(int)tmap.sensorX[i];
					else x1=(int)tmap.neutralX[i-tmap.resolution*tmap.sensorRes];
				
					if (i<tmap.resolution*tmap.sensorRes) y1=(int)tmap.sensorY[i];
					else y1=(int)tmap.neutralY[i-tmap.resolution*tmap.sensorRes];
					
					if (j<tmap.resolution*tmap.sensorRes) x2=(int)tmap.sensorX[j];
					else x2=(int)tmap.neutralX[j-tmap.resolution*tmap.sensorRes];
				
					if (j<tmap.resolution*tmap.sensorRes) y2=(int)tmap.sensorY[j];
					else y2=(int)tmap.neutralY[j-tmap.resolution*tmap.sensorRes];
				
					g.drawLine(offsetX+size*x1, offsetX+300+size*y1,offsetX+size*x2, offsetX+300+size*y2);
				
				}
			}
		}/**/
		
		
		
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
		}/**/
		
		
		
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
		
		/*
		// draw sensor change sequence
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			
			if (tmap.m_tactilePressure[i]>0){
				if (tmap.timerMap[i]==20) sensorSequence[i][index]=Color.cyan;
				else					  sensorSequence[i][index]=Color.orange;
			}
			else{
				if (tmap.lastAction==0){
					sensorSequence[i][index]=new Color( tmap.actionValue/1.5f,0,0);
				}
				if (tmap.lastAction==1){
					sensorSequence[i][index]=new Color( 0,-tmap.actionValue/0.8f,0);
				}
				if (tmap.lastAction==2){
					sensorSequence[i][index]=new Color( 0,0,-tmap.actionValue/0.8f);
				}
			}
			
			for (int j=0;j<800;j++){
				g.setColor(sensorSequence[i][j]);
				g.fillRect(300+i*5, j, 5, 1);
			}
			g.setColor(Color.green);
			g.drawLine(300, index+1, 570, index+1);
		}/**/

		
		// draw state links
		for (int l=0;l<tmap.stateLink.size();l++){
			int y=l%400;
			int x=(l-y)/400;
			for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
				//if (tmap.stateLink.get(l)[0][i]==1) g.setColor(Color.red);
				//else                                g.setColor(Color.green);
				//g.fillRect(300+2*i+120*x, 5+y*4, 2, 1);
				
				if (tmap.stateLink.get(l)[1][i]==1) g.setColor(Color.red);
				else                                g.setColor(Color.green);
				g.fillRect(300+2*i+120*x, 5+y*2, 2, 2);
				
			}
		}
		int y=tmap.actualState%400;
		int x=(tmap.actualState-y)/400;
		g.setColor(Color.blue);
		g.fillRect(295+120*x, 5+y*2, 2, 2);
		
		
		
		/*
		// draw previous state
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				
				for (int k=0;k<3;k++){
					for (int l=0;l<50;l++){
						g.setColor(new Color( Math.max(0, tmap.previousState[i][j][k*50+l]), Math.max(0, -tmap.previousState[i][j][k*50+l]),0));
						g.fillRect(300+k*20 + (k*50+l)*2, 2+(i*tmap.resolution*tmap.sensorRes+j)+i*2, 2, 1);
					}
				}
				
			}
		}/**/
		
		/*
		// draw previous state 2
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				
				for (int k=0;k<3;k++){
					g.setColor(new Color( Math.max(0, -tmap.previousState1[i][j][k]), Math.max(0, tmap.previousState1[i][j][k]),0));
					g.fillRect(300+j*3+k*200, 50+i*4,3,4);
					
					
					g.setColor(new Color( Math.max(0, -tmap.previousState0[i][j][k]), Math.max(0, tmap.previousState0[i][j][k]),0));
					g.fillRect(300+j*3+k*200, 400+i*4,3,4);
				}
				
			}
		}/**/
		
		/*
		size=10;
		g.setColor(Color.black);
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			g.fillOval(offsetX+400+(int)(tmap.sensorX[i]*size)-2, offsetX+    (int)(tmap.sensorY[i]*size)-2, 4, 4);
			g.fillOval(offsetX+    (int)(tmap.sensorX[i]*size)-2, offsetX+400+(int)(tmap.sensorY[i]*size)-2, 4, 4);
			g.fillOval(offsetX+400+(int)(tmap.sensorX[i]*size)-2, offsetX+400+(int)(tmap.sensorY[i]*size)-2, 4, 4);
		}

		// draw points neighbor links
		double x,y;
		double d;
		for (int i=0;i<tmap.resolution*tmap.sensorRes;i++){
			for (int j=0;j<tmap.resolution*tmap.sensorRes;j++){
				if (i!=j){
				
					d=Math.sqrt( (tmap.sensorX[j]-tmap.sensorX[i])*(tmap.sensorX[j]-tmap.sensorX[i])
							    +(tmap.sensorY[j]-tmap.sensorY[i])*(tmap.sensorY[j]-tmap.sensorY[i]));
					
					for (int k=0;k<3;k++){
						double p=0;
						if (tmap.previousState1[i][j][k]>0) p=tmap.previousState1[i][j][k];

						x=(tmap.sensorX[j]-tmap.sensorX[i])*2/d *p + tmap.sensorX[i];
						y=(tmap.sensorY[j]-tmap.sensorY[i])*2/d *p + tmap.sensorY[i];
						if (k==0){
							g.setColor(Color.red);
							g.drawLine(offsetX+400+(int)(tmap.sensorX[i]*size), offsetX+(int)(tmap.sensorY[i]*size),
								       offsetX+400+(int)(x*size), offsetX+(int)(y*size));
						}
						if (k==1){
							g.setColor(Color.green);
							g.drawLine(offsetX+(int)(tmap.sensorX[i]*size), offsetX+400+(int)(tmap.sensorY[i]*size),
								       offsetX+(int)(x*size), offsetX+400+(int)(y*size));
						}
						if (k==2){
							g.setColor(Color.blue);
							g.drawLine(offsetX+400+(int)(tmap.sensorX[i]*size), offsetX+400+(int)(tmap.sensorY[i]*size),
								       offsetX+400+(int)(x*size), offsetX+400+(int)(y*size));
						}
					
						
					}
				}
			}
		}/**/
		
		
		index++;
		if (index>=800) index=0;
	}
	
}

