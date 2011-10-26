import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

/**
 * Panel used to display informations concerning colliculus
 * @author simon
 */
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
		

		
		double Sum0,Sum1,countD,d;
		float max=0;
		int red=0,green,blue;
		for (int i=0;i<180;i++){
			for (int j=0;j<100;j++){
				//----------------------------------------------------------------
				// tactile charge map in Polar referential
				offsetX=500;
				offsetY=150;
				g.setColor(new Color( Math.min(1,colliculus.tmap.chargeMapP[i][j][2]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][1]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][0])) );
					
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + (100-j)*width,
						   width, width);
				/**/
				
				//--------------------------------------------------------------
				// visual colliculus in polar referential
				offsetX=500;
				offsetY=500;
				max=0;
				for (int k=2;k<10;k++){
					if (colliculus.vmap.chargeMapP[i][j][k]>max) max=colliculus.vmap.chargeMapP[i][j][k];
				}
				g.setColor(new Color(max,
									 colliculus.vmap.chargeMapP[i][j][1],
									 colliculus.vmap.chargeMapP[i][j][0]));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
				//--------------------------------------------------------------
				// tactile maximum charges in polar referential
				/*offsetX=500;
				offsetY=150;
				red=0;
				green=0;
				blue=0;
				if (colliculus.vmap.maximumMap[i][j]==0) blue=250;
				if (colliculus.vmap.maximumMap[i][j]==1) green=250;
				if (colliculus.tmap.maximumMap[i][j]==2) red=250;

				g.setColor(new Color(red,green,blue));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
				//--------------------------------------------------------------
				// visual maximum charges in polar referential
				/*offsetX=500;
				offsetY=500;
				max=0;
				red=0;
				green=0;
				blue=0;
				if (colliculus.vmap.maximumMap[i][j]==0) blue=250;
				if (colliculus.vmap.maximumMap[i][j]==1) green=250;
				if (colliculus.vmap.maximumMap[i][j]>1 ) red=250;
				
				g.setColor(new Color(red,green,blue));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
				
				
				
			}
			
			//--------------------------------------------------------------
			// draw tactile output vector
			/*offsetX=500;
			offsetY=150;
			g.setColor(Color.cyan);
			if (colliculus.tmap.output[i][0]>0){
				for (int j=colliculus.tmap.output[i][2];j<=colliculus.tmap.output[i][3];j++){
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + (100-j)*width,
							   width, width);
				}
			}/**/
			
			//--------------------------------------------------------------
			// draw visual output vector
			/*offsetX=500;
			offsetY=500;
			g.setColor(Color.cyan);
			if (colliculus.vmap.output[i][0]>0){
				for (int j=colliculus.vmap.output[i][2];j<=colliculus.vmap.output[i][3];j++){
					g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
							   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width,
							   width, width);
				}
			}/**/
		}
		
		/*colliculus.getSalienceList();

		g.setColor(Color.yellow);
		for (int i=0;i<colliculus.liste.size();i++){
			if (colliculus.liste.get(i).getType()==0){
				offsetX=500;
				offsetY=150;
				g.drawLine(offsetX-colliculus.vmap.mapSize/2*width 
					        	+ (int)(((colliculus.liste.get(i).getTheta()
					        	- colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
					       offsetY-colliculus.vmap.mapSize/2*width 
					   			+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width,
					   			
					       offsetX-colliculus.vmap.mapSize/2*width 
				        		+ (int)(((colliculus.liste.get(i).getTheta()
				        		+ colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
				           offsetY-colliculus.vmap.mapSize/2*width 
					   			+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width);
			}
			if (colliculus.liste.get(i).getType()==1){
				offsetX=500;
				offsetY=500;
				
				g.drawLine(offsetX-colliculus.vmap.mapSize/2*width 
					        	+ (int)(((colliculus.liste.get(i).getTheta()
					        	-colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
					       offsetY-colliculus.vmap.mapSize/2*width 
					       		+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width,
					   			
					       offsetX-colliculus.vmap.mapSize/2*width 
				        		+ (int)(((colliculus.liste.get(i).getTheta()
				        		+colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
				           offsetY-colliculus.vmap.mapSize/2*width 
				           		+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width);
			}
		}/**/
		
		
	}
}
