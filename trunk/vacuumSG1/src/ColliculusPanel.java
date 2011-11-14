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
		offsetX=160;
		offsetY=170;
		g.setColor(Color.black);
		g.drawString("tactile cartesian", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		offsetX=160;
		offsetY=500;
		g.setColor(Color.black);
		g.drawString("visual cartesian", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				//--------------------------------------------------------------
				// tactile colliculus in Cartesian referential
				offsetX=160;
				offsetY=170;
				g.setColor(new Color(Math.min(1,colliculus.tmap.chargeMap1[i][j][3]),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][1]),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][0])) );

				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				/* */
				//---------------------------------------------------------------
				// visual colliculus in Cartesian referential
				offsetX=160;
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
		
		
		// draw bundle connections
		offsetX=500;
		offsetY=500;
		g.setColor(new Color(0,0,128));
		g.fillRect(offsetX, offsetY-20, 18, 18);
		g.setColor(new Color(0,128,0));
		g.fillRect(offsetX+20, offsetY-20, 18, 18);
		g.setColor(new Color(115,230,0));
		g.fillRect(offsetX+40, offsetY-20, 18, 18);
		g.setColor(new Color(150,128,255));
		g.fillRect(offsetX+60, offsetY-20, 18, 18);
		g.setColor(new Color(46,230,0));
		g.fillRect(offsetX+80, offsetY-20, 18, 18);
		g.setColor(new Color(0,230,230));
		g.fillRect(offsetX+100, offsetY-20, 18, 18);
		g.setColor(new Color(0,230,92));
		g.fillRect(offsetX+120, offsetY-20, 18, 18);
		g.setColor(new Color(230,207,0));
		g.fillRect(offsetX+140, offsetY-20, 18, 18);
		g.setColor(new Color(0,230,161));
		g.fillRect(offsetX+160, offsetY-20, 18, 18);
		g.setColor(new Color(184,230,0));
		g.fillRect(offsetX+180, offsetY-20, 18, 18);
		
		g.setColor(Color.blue);
		g.fillRect(offsetX-20, offsetY, 18, 18);
		g.setColor(Color.green);
		g.fillRect(offsetX-20, offsetY+30, 18, 18);
		g.setColor(Color.red);
		g.fillRect(offsetX-20, offsetY+60, 18, 18);
		g.setColor(Color.magenta);
		g.fillRect(offsetX-20, offsetY+90, 18, 18);
		
		g.setColor(Color.red);
		for (int i=0;i<10;i++){
			for (int j=0;j<4;j++){
				if (colliculus.bundles[i][j]==1){
					g.fillRect(offsetX+i*20, offsetY+j*30, 18, 18);
				}
			}
		}
		
		
		double Sum0,Sum1,countD,d;
		float max=0;
		int red=0,green,blue;
		offsetX=500;
		offsetY=170;
		g.setColor(Color.black);
		g.drawString("tactile polar", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		offsetX=500;
		offsetY=500;
		g.setColor(Color.black);
		g.drawString("visual polar", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		for (int i=0;i<180;i++){
			for (int j=0;j<100;j++){
				//----------------------------------------------------------------
				// tactile charge map in Polar referential
				offsetX=500;
				offsetY=170;
				g.setColor(new Color( Math.min(1,colliculus.tmap.chargeMapP[i][j][2]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][1]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][0])) );
					
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + (100-j)*width,
						   width, width);
				/**/
				
				
				//--------------------------------------------------------------
				// visual colliculus in polar referential
				/*offsetX=500;
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
			// projection of the visual colliculus
			offsetX=500;
			offsetY=500;
			max=0;
			
			
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
		
		//-----------------------------------------------------------------
		// projection of the visual colliculus4
		/*
		offsetX=500;
		offsetY=500;
		
		g.setColor(Color.BLUE);
    	g.fillRect(offsetX-colliculus.vmap.mapSize/2*width,
    			   offsetY-150,180*width,150);
    	
    	g.setColor(Color.orange);
    	g.fillRect(offsetX-colliculus.vmap.mapSize/2*width,
    			   offsetY,180*width,150);
		for (int i=0;i<180;i++){
			if (colliculus.vmap.output[i][0]>0){
				int val=colliculus.vmap.output[i][0];
				if      (val==1) g.setColor(new Color(0,128,0));
				else if (val==2) g.setColor(new Color(115,230,0));
				else if (val==3) g.setColor(new Color(150,128,255));
				else if (val==4) g.setColor(new Color(46,230,0));
				else if (val==5) g.setColor(new Color(0,230,230));
				else if (val==6) g.setColor(new Color(0,230,92));
				else if (val==7) g.setColor(new Color(230,207,0));
				else if (val==8) g.setColor(new Color(0,230,161));
				else if (val==9) g.setColor(new Color(184,230,0));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY - Math.min(100,1000/colliculus.vmap.output[i][1]),
						   width, 2*Math.min(100,1000/colliculus.vmap.output[i][1]));
			}
		}
		g.setColor(Color.red);
		g.drawLine(offsetX-colliculus.vmap.mapSize/2*width+45*width,
				   offsetY-150,
				   offsetX-colliculus.vmap.mapSize/2*width+45*width,
				   offsetY+150);
		
		g.drawLine(offsetX-colliculus.vmap.mapSize/2*width+135*width,
				   offsetY-150,
				   offsetX-colliculus.vmap.mapSize/2*width+135*width,
				   offsetY+150);
		/* */
		
		/*
		colliculus.getSalienceList();

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
