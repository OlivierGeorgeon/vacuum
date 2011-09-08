import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;


public class PatternMappingPanel extends JPanel{
	
	private PatternMap patternMap;
	public int x,y,theta;
	public int [][][] map3D;
	public float [][][] distance3D;
	
	public PatternMappingPanel(PatternMap p){
		patternMap=p;
		x=0;
		y=0;
		theta=0;
		map3D=new int[180][140][360];
		distance3D=new float[180][140][360];
		for (int i=0;i<180;i++){
			for (int j=0;j<140;j++){
				for (int k=0;k<360;k++){
					map3D[i][j][k]=-1;
					distance3D[i][j][k]=-1;
				}
			}
		}
	}
	
	public void setMap(){
		
		int th;
		float d;
		for (int i=-50;i<50;i++){
			if (x+i>=0 && x+i<180){
				for (int j=-50;j<50;j++){
					if (y+j>=0 && y+j<140){
						for (int k=-50;k<50;k++){
							th=(theta+k+360)%360;
							
							// compute distance
							d=(float) Math.sqrt( (i*i)+(j*j)+(k*k) );
							
							if (d<=50 && (d<=distance3D[x+i][y+j][th] || distance3D[x+i][y+j][th]==-1)){
								map3D[x+i][y+j][th]=patternMap.actualIndex;
								distance3D[x+i][y+j][th]=d;
							}
							
						}
					}
				}
			}
		}
	}
	
	
	public void paintPatternMap(Graphics g){
		// display pattern maps (8-directions)
		g.setColor(Color.black);
		g.drawRect(250, 10, 180, 150);	// top left
		g.drawRect(440, 10, 180, 150);	// top
		g.drawRect(630, 10, 180, 150);	// top right
		
		g.drawRect(250, 170, 180, 150);	// left
		g.drawRect(630, 170, 180, 150);	// right
		
		g.drawRect(250, 330, 180, 150);	// bottom left
		g.drawRect(440, 330, 180, 150); // bottom
		g.drawRect(630, 330, 180, 150);	// bottom right
		
		g.setColor(patternMap.patternColor.get(patternMap.actualIndex));
		if (theta<23 || theta>=338) g.fillOval(440+x-5, 10+y-5, 10, 10);	// top
		if (theta>=23 && theta<68) g.fillOval(630+x-5, 10+y-5, 10, 10);		// top right
		
		if (theta>=68 && theta<113) g.fillOval(630+x-5, 170+y-5, 10, 10);	// right
		if (theta>=113 && theta<158) g.fillOval(630+x-5, 330+y-5, 10, 10);	// bottom right
		
		if (theta>=158 && theta<203) g.fillOval(440+x-5, 330+y-5, 10, 10);	// bottom
		if (theta>=203 && theta<248) g.fillOval(250+x-5, 330+y-5, 10, 10);	// bottom left
		
		if (theta>=248 && theta<293) g.fillOval(250+x-5, 170+y-5, 10, 10);	// left
		if (theta>=293 && theta<338) g.fillOval(250+x-5,  10+y-5, 10, 10);	// top left
	}
	
	
	public void paint3DMap(Graphics g){
		// draw axis
		g.setColor(Color.black);
		g.drawLine(530, 370, 530, 9);
		g.drawLine(530, 370, 260, 640);
		g.drawLine(530, 370, 1250, 370);
		
		g.drawLine(530, 370-theta, 1250, 370-theta);
		g.drawLine(530, 370-theta, 260, 650-theta);
		// display the 3D map
		for (int k=0;k<360;k++){
			for (int i=0;i<180;i++){
				for (int j=0;j<140;j++){
					if (map3D[i][j][k]!=-1 && map3D[i][j][k]==patternMap.actualIndex){
						if (k==theta) g.setColor(Color.black);
						else          g.setColor(patternMap.patternColor.get(map3D[i][j][k]));
						g.fillRect(250+i*4+(280-j*2), 10+j*2+(360-k), 4, 2);
					}
				}
			}
		}
		/*for (int i=359;i>=theta;i--){
			if (map3D[x][y][i]!=-1 )g.setColor(patternMap.patternColor.get(map3D[x][y][i]));
			else g.setColor(new Color(200,200,200));
			g.fillRect(250+x*4+(280-y*2), 10+y*2+(360-i), 4, 2);
		}*/
		g.setColor(Color.black);
		g.drawLine(260, 650-theta, 1010, 650-theta);
		g.drawLine(1010, 650-theta, 1250, 370-theta);
	}
	
	
	
	public void paintObjects(Graphics g){
		for (int k=0;k<360;k++){
			for (int i=0;i<180;i++){
				for (int j=0;j<140;j++){
					boolean test=false;
					if (map3D[i][j][k]!=-1)
						test= patternMap.patternList.get(map3D[i][j][k]).get(0).equals(new Color(184,230,0));
					if (test && map3D[i][j][k]!=-1 ){
						g.setColor(new Color((int)(k*0.7),(int)(255-k*0.7),0));
						g.fillRect(250+i*4, 10+j*4, 4, 4);
					}
				}
			}
		}
	}
	
	public void paintNodes(Graphics g){
		g.setColor(Color.black);
		g.drawRect(260, 10, 500, 500);
		
		// draw links
		g.setColor(Color.red);
		for (int i=0;i<patternMap.nodeList.size();i++){
			for (int j=0;j<patternMap.nodeList.size();j++){
				if (i!=j  &&  patternMap.nodeList.get(j).output.contains(i)){
					g.drawLine((int)(patternMap.nodeList.get(i).x+262), (int)(patternMap.nodeList.get(i).y+12),
							   (int)(patternMap.nodeList.get(j).x+262), (int)(patternMap.nodeList.get(j).y+12));
				}
			}
		}
		
		// draw nodes
		for (int i=0;i<patternMap.nodeList.size();i++){
			g.setColor(patternMap.patternColor.get(patternMap.nodeList.get(i).id));
			g.fillOval((int)(patternMap.nodeList.get(i).x+260), (int)(patternMap.nodeList.get(i).y+10), 5, 5);
			if (i==patternMap.actualNode){
				g.setColor(Color.green);
				g.drawOval((int)(patternMap.nodeList.get(i).x+259), (int)(patternMap.nodeList.get(i).y+9), 7, 7);
			}
		}
		
		// draw actual node
		int actual=patternMap.actualNode;
		int nbOutput=patternMap.nodeList.get(actual).output.size();
		int outputH=240/(nbOutput+1);
		
		
		g.setColor(patternMap.patternColor.get(actual));
		g.drawOval(850, 50, 300, 300);
		//g.drawRect(880, 80, 240, 240);
		
		// draw output nodes
		for (int i=0;i<nbOutput;i++){
			g.setColor(patternMap.patternColor.get(patternMap.nodeList.get(actual).output.get(i)));
			g.fillRect(1100, 80+(i+1)*outputH, 3, 3);
		}
	}
	
	
	public void paintComponent(Graphics g){
		
		g.setColor(Color.white);
		g.fillRect(0, 0, 1300, 700);
		
		int nb,width;
		
		// display pattern
		for (int i=0;i<patternMap.patternList.size();i++){
			nb=patternMap.patternList.get(i).size();
			width=200/nb;
			
			g.setColor(patternMap.patternColor.get(i));
			g.fillRect(0, 10+i*5, 10, 4);
			
			for (int j=0;j<nb;j++){
				g.setColor(patternMap.patternList.get(i).get(j));
				g.fillRect(20+j*width, 10+i*5, width, 4);
			}
			if (i==patternMap.actualIndex){
				g.setColor(Color.red);
				g.fillRect(252, 10+i*5, 5, 4);
			}
		}
		
		paintNodes(g);
	
	}

}
