import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class VisualMapPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	VisualMap tmap;
	public int width;
	
	
	public VisualMapPanel(VisualMap v){
		tmap=v;
		width=200/(40-10);//tmap.mapSize;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 800, 800);
		
		int offsetX,offsetY;
		
		for (int i=0;i<50;i++){
			for (int j=0;j<26;j++){
				// draw charge map
				g.setColor(new Color(Math.min(1,tmap.chargeMap0[i][j]),1-Math.min(1,tmap.chargeMap0[i][j]),0));
				g.fillRect(50+ i*width, 50+ j*width, width, width);
			}
		}
		
		
		for (int k=0;k<Math.min(3, tmap.flowX1.size());k++){
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
			for (int i=0;i<50;i+=2){
				for (int j=0;j<50;j+=2){

					// draw improved flow

					if (tmap.testMap[i][j]){
						//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
						g.setColor(Color.blue);
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
		

	}
	
}

