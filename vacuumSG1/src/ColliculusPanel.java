import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class ColliculusPanel extends JPanel{
	
	public Colliculus colliculus;
	public int width;
	
	public ColliculusPanel(Colliculus c){
		colliculus=c;
		width=100/(40-10);
	}
	
	
	
	public void paintComponent(Graphics g){
		
		g.setColor(Color.white);
		g.fillRect(0, 0, 800, 800);
		
		float x,y;
		
		/*
		for (int i=0;i<colliculus.tmap.resolution;i++){
			g.setColor(Color.blue);
			g.drawLine((int)(colliculus.tmap.sensorX[i]*2)+150, (int)(colliculus.tmap.sensorY[i]*2)+150,
					   (int)(colliculus.tmap.sensorX[i+colliculus.tmap.resolution]*2)+150,
					   (int)(colliculus.tmap.sensorY[i+colliculus.tmap.resolution]*2)+150);
			
			g.drawLine((int)(colliculus.tmap.sensorX[i+colliculus.tmap.resolution]*2)+150,
					   (int)(colliculus.tmap.sensorY[i+colliculus.tmap.resolution]*2)+150,
					   (int)(colliculus.tmap.sensorX[i+colliculus.tmap.resolution*2]*2)+150,
					   (int)(colliculus.tmap.sensorY[i+colliculus.tmap.resolution*2]*2)+150);
			
			g.setColor(Color.red);
			g.fillOval((int)(colliculus.tmap.valueX[i]*2)+150,
					   (int)(colliculus.tmap.valueY[i]*2)+150,
					   3, 3);
		}*/
		int offsetX,offsetY;
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				// tactile potential map
				offsetX=150;
				offsetY=150;
				/*g.setColor(new Color(  Math.min(1,colliculus.tmap.potentialMap[i][j]),
									 1-Math.min(1,colliculus.tmap.potentialMap[i][j]),
									 0));
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				*/
				//--------------------------------------------------------------
				// tactile colliculus in Cartesian referential
				offsetX=150;
				offsetY=150;
				g.setColor(new Color(Math.min(1,colliculus.tmap.chargeMap1[i][j][2]),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][1]),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][0])) );

				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				
				//---------------------------------------------------------------
				// visual colliculus in Cartesian referential
				offsetX=500;
				offsetY=150;
				g.setColor(new Color(0,
									   colliculus.vmap.chargeMap0[i][j],
									 1-colliculus.vmap.chargeMap0[i][j]));
				
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + j*width, 
						   width, width);
				
			}
		}
		
		/*
		g.setColor(Color.lightGray);
		g.fillOval((50)*width, (50)*width, 2*width,2*width );
		
		g.setColor(Color.lightGray);
		g.fillOval(500-colliculus.vmap.mapSize/2*width + 24*width,  150-colliculus.vmap.mapSize/2*width + 24*width, 3*width,3*width );
		*/
		
		//----------------------------------------------------------------
		// charge map in Polar referential
		double Sum0,Sum1,countD,d;
		for (int i=0;i<90;i++){
			for (int j=0;j<30;j++){
				g.setColor(new Color(   Math.min(1,colliculus.tmap.chargeMapP[i][j][0]),
										Math.min(1,colliculus.tmap.chargeMapP[i][j][1]),
									  1-Math.min(1,Math.max(colliculus.tmap.chargeMapP[i][j][0],
														    colliculus.tmap.chargeMapP[i][j][1]))) );
					
				g.fillRect(150-colliculus.tmap.mapSize/2*width + i*width,
						   450-colliculus.tmap.mapSize/2*width + (30-j)*width,
						   width, width);
			}
		}
		
		//--------------------------------------------------------------
		// visual colliculus in polar referential
		for (int i=0;i<180;i++){
			for (int j=0;j<50;j++){
				
				g.setColor(new Color(  colliculus.vmap.potentialMap[i][j],
						             1-colliculus.vmap.potentialMap[i][j],
						             0) );
				
				//if (colliculus.vmap.testMap[i][j]) g.setColor(Color.red);
				//else g.setColor(Color.green);
				g.fillRect((i+90)*width/4, 500+(50-j)*4, width/4+1, 4);
			}
		}
		
	}
}
