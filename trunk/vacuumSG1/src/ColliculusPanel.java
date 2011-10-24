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
		g.fillRect(0, 0, 1000, 800);
		
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
				offsetX=150;
				offsetY=500;
				float max=0;
				for (int k=2;k<10;k++){
					if (colliculus.vmap.chargeMap1[i][j][k]>max) max=colliculus.vmap.chargeMap1[i][j][k];
				}
				g.setColor(new Color(max,
									 colliculus.vmap.chargeMap1[i][j][1],
									 colliculus.vmap.chargeMap1[i][j][0]));
				
				
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
		// tactile charge map in Polar referential
		offsetX=500;
		offsetY=360;
		double Sum0,Sum1,countD,d;
		for (int i=0;i<180;i++){
			for (int j=0;j<100;j++){
				g.setColor(new Color( Math.min(1,colliculus.tmap.chargeMapP[i][j][2]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][1]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][0])) );
					
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + (30-j)*width,
						   width, width);
			}
		}
		
		//--------------------------------------------------------------
		// visual colliculus in polar referential
		for (int i=0;i<180;i++){
			for (int j=0;j<100;j++){
				offsetX=500;
				offsetY=710;
				float max=0;
				for (int k=2;k<10;k++){
					if (colliculus.vmap.chargeMapP[i][j][k]>max) max=colliculus.vmap.chargeMapP[i][j][k];
				}
				g.setColor(new Color(max,
									 colliculus.vmap.chargeMapP[i][j][1],
									 colliculus.vmap.chargeMapP[i][j][0]));
				
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (30-j)*width, 
						   width, width);
			}
		}
		
	}
}
