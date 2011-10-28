import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class VisualMapPanel extends JPanel{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	VisualMap vmap;
	public int width;
	
	
	public VisualMapPanel(VisualMap v){
		vmap=v;
		width=100/(40-10);//tmap.mapSize;
	}
	
	public void paintComponent(Graphics g){
		g.setColor(Color.white);
		g.fillRect(0, 0, 800, 800);
		
		int offsetX,offsetY;
		
		for (int i=0;i<180;i++){
			for (int j=0;j<70;j++){
				// draw charge map
				//g.setColor(new Color(Math.min(1,vmap.potentialMap[i][j]),1-Math.min(1,vmap.potentialMap[i][j]),0));
				g.setColor(new Color(vmap.timerMap[i][j]*5,(40-vmap.timerMap[i][j])*5,0));
				g.fillRect(10+ i*width, 10+ (70-j)*width, width*2, width*2);
			}
		}
		
		
		for (int k=0;k<Math.min(3, vmap.flowX1.size());k++){
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
			/*
			for (int i=0;i<100;i+=8){
				for (int j=0;j<100;j+=8){

					// draw improved flow

					//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
					g.setColor(Color.blue);
					g.drawLine(offsetX-vmap.mapSize/2*width + i*width+width,
							   offsetY-vmap.mapSize/2*width + j*width+width/2,
							   offsetX-vmap.mapSize/2*width + i*width+width+(int)(1000*vmap.flowX3.get(k)[i][j]),
							   offsetY-vmap.mapSize/2*width + j*width+width/2+(int)(1000*vmap.flowY3.get(k)[i][j]) );
				}
			}*/
			
			/*
			for (int i=0;i<180;i+=6){
				for (int j=0;j<70;j+=6){
					int lenght=40;
					if (k>0) lenght=100;
					// draw improved flow

					//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
					g.setColor(Color.blue);
					g.drawLine(offsetX-vmap.mapSize/2*width + i*width/2+width,
							   offsetY-vmap.mapSize/2*width + (70-j)*width+width/2,
							   offsetX-vmap.mapSize/2*width + i*width/2+width+(int)(lenght*vmap.flowX2.get(k)[i][j]),
							   offsetY-vmap.mapSize/2*width + (70-j)*width+width/2+(int)(lenght*vmap.flowY2.get(k)[i][j]) );
				}
			}/* */
			
			
			for (int i=0;i<100;i+=4){
				for (int j=0;j<100;j+=4){

					// draw improved flow

					//g.setColor(new Color(Math.min(1,tmap.potentialMap[i][j]),1-Math.min(1,tmap.potentialMap[i][j]),0));
					g.setColor(Color.blue);
					g.drawLine(offsetX-vmap.mapSize/2*width + i*width+width,
							   offsetY-vmap.mapSize/2*width + j*width+width,
							   offsetX-vmap.mapSize/2*width + i*width+width+(int)(500*vmap.flowX3.get(k)[i][j]),
							   offsetY-vmap.mapSize/2*width + j*width+width+(int)(500*vmap.flowY3.get(k)[i][j]) );
				}
			}/* */
		}
		

	}
	
}

