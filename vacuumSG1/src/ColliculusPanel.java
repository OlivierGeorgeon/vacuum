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
		
		for (int i=0;i<50;i++){
			for (int j=0;j<50;j++){
			
				// potential map
				//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
				//g.fillRect(450-tmap.mapSize/2*width + i*width, 450-tmap.mapSize/2*width + j*width, width, width);

				// charge map in Cartesian referential
				g.setColor(new Color(Math.min(1,colliculus.tmap.chargeMap0[i][j][0]),
						             Math.min(1,colliculus.tmap.chargeMap0[i][j][1]),
						             1-Math.min(1,Math.max(colliculus.tmap.chargeMap0[i][j][0],
						            		               colliculus.tmap.chargeMap0[i][j][1]))) );
				g.fillRect(150-colliculus.tmap.mapSize/2*width + i*width, 150-colliculus.tmap.mapSize/2*width + j*width, width, width);
			}
		}
		
		double Sum0,Sum1,countD,d;
		float x,y;
		for (int i=0;i<90;i++){
			for (int j=0;j<30;j++){
				// charge map in Polar referential
				
				x=(float) ((double)j*Math.cos( ((double)i*4+90)*Math.PI/180))+25;
				y=(float) ((double)j*Math.sin( ((double)i*4+90)*Math.PI/180))+25;
				
				int ix=Math.round(x);
				int jy=Math.round(y);
				
				if (ix>=0 && jy>=0 && ix<50 && jy<50){
				
					Sum0=0;
					Sum1=0;
					countD=0;
					for (int i2=-1;i2<=1;i2++){
						for (int j2=-1;j2<=1;j2++){
							if (ix+i2>=0 && ix+i2<50 && jy+j2>=0 && jy+j2<50){
								d= ((float)(ix+i2)-x)*((float)(ix+i2)-x) 
									+((float)(jy+j2)-y)*((float)(jy+j2)-y);
								d=Math.min(1,Math.sqrt(d));
								Sum0+=colliculus.tmap.chargeMap0[ix+i2][jy+j2][0]*(1-d);
								Sum1+=colliculus.tmap.chargeMap0[ix+i2][jy+j2][1]*(1-d);
								countD+=(1-d);
							}
						}
					}
				
					g.setColor(new Color(Math.min(1,(float)(Sum0/countD)),
										Math.min(1,(float)(Sum1/countD)),
										1-Math.min(1,Math.max((float)(Sum0/countD),
						            		               	  (float)(Sum1/countD)))) );
				}
				else g.setColor(Color.black);
				g.fillRect(150-colliculus.tmap.mapSize/2*width + i*width, 500-colliculus.tmap.mapSize/2*width + (30-j)*width, width, width);
			
			}
		}
		
		for (int i=0;i<45;i++){
			for (int j=0;j<50;j++){
				
				if (colliculus.vmap.confidenceMap[i*4][j]!=-1){
					g.setColor(colliculus.vmap.colorMap[i*4][j]);
					//g.setColor(new Color(0,(int)(colliculus.vmap.confidenceMap[i*4][j]*10),(int)(200-colliculus.vmap.confidenceMap[i*4][j]*10) ));
				}
				else g.setColor(Color.white);
				
				g.fillRect((i+22)*width, 550+(50-j)*4, width, 4);
			}
		}
		
	}
}
