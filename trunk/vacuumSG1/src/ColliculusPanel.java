import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class ColliculusPanel extends JPanel{
	
	public Colliculus colliculus;
	public int width;
	
	public ColliculusPanel(Colliculus c){
		colliculus=c;
		width=200/(40-10);
	}
	
	
	
	public void paintComponent(Graphics g){
		
		g.setColor(Color.white);
		g.fillRect(0, 0, 800, 800);
		
		float x,y;
		
		
		for (int i=0;i<50;i++){
			for (int j=0;j<50;j++){
				// potential map
				g.setColor(new Color(  Math.min(1,colliculus.tmap.potentialMap[i][j]),
									 1-Math.min(1,colliculus.tmap.potentialMap[i][j]),
									 0));
				g.fillRect(150-colliculus.tmap.mapSize/2*width + i*width,
						   150-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				
				//--------------------------------------------------------------
				// tactile colliculus in Cartesian referential
				/*g.setColor(new Color(Math.min(1,colliculus.tmap.chargeMap0[i][j][0]),
						             Math.min(1,colliculus.tmap.chargeMap0[i][j][1]),
						             Math.min(1,colliculus.tmap.chargeMap0[i][j][2])) );

				g.fillRect(150-colliculus.tmap.mapSize/2*width + i*width,
						   150-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				*/
				
				//---------------------------------------------------------------
				// visual colliculus in Cartesian referential
				g.setColor(new Color(0,
									   colliculus.vmap.chargeMap0[i][j],
									 1-colliculus.vmap.chargeMap0[i][j]));
				
				
				g.fillRect(500-colliculus.vmap.mapSize/2*width + i*width,
						   150-colliculus.vmap.mapSize/2*width + j*width, 
						   width, width);
			}
		}
		
		
		g.setColor(Color.lightGray);
		g.fillOval((24)*width, (24)*width, 3*width,3*width );
		
		g.setColor(Color.lightGray);
		g.fillOval(500-colliculus.vmap.mapSize/2*width + 24*width,  150-colliculus.vmap.mapSize/2*width + 24*width, 3*width,3*width );
		
		
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
