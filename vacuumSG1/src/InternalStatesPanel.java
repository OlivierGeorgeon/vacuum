import java.awt.Color;
import java.awt.Graphics;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.util.ArrayList;

import javax.imageio.ImageIO;
import javax.imageio.stream.FileImageOutputStream;
import javax.swing.JPanel;


public class InternalStatesPanel extends JPanel{

	private ArrayList<Action> actList;
	private int distance;
	
	public InternalStatesPanel(ArrayList<Action> aList){
		actList=aList;
	}

	
	
	public void draw2D(Graphics g,Action act1,int obj1,int input1,int output1,
			                      Action act2,int obj2,int input2,int output2){
		
		float h1=act1.height;
		float w1=act1.width;

		float r1=Math.min(580/h1,280/w1);
		
		float h2=act2.height;
		float w2=act2.width;
		
		float r2=Math.min(580/h2,280/w2);
		
		////////////////////////////////////////////
		// object 1
		if (act1.selectMap.size()>obj1){
			for (int i=0;i<w1;i++){
				for (int j=0;j<h1;j++){
					if (j==input1 || i==output1) g.setColor(Color.blue);
					else           g.setColor(new Color(  (100-act1.selectMap.get(obj1)[i][j])/200 ,
														  (100+act1.selectMap.get(obj1)[i][j])/200 ,
						                                  (Math.min(10, act1.confidenceMap.get(obj1)[i][j]))/10));
					g.fillRect(10+(int)(j*r1), 290-(int)(i*r1), (int)r1+1, (int)r1+1);
				}
			}
			
			for (int i=0;i<act1.links.size();i++){
				g.setColor(act1.objMemory.objectList.get(i));
				g.fillRect(370, i*20+10, (int)(act1.links.get(i).get(obj1)*200), 19);
			}
		}
		
		// grid
		g.setColor(Color.black);
		for (int i=0;i<h1;i+=20){
			g.drawLine(10+(int)(i*r1), 290, 10+(int)(i*r1), 290-(int)(w1*r1));
		}
		for (int i=0;i<w1;i+=20){
			g.drawLine(10,290-(int)(i*r1), 10+(int)(h1*r1), 290-(int)(i*r1));
		}
		
		//g.setColor(Color.blue);
		//if (act1.previousObj==obj1) g.drawRect(5,5,590,290);
		
		///////////////////////////////////////////////
		// object 2
		if (act2.selectMap.size()>obj2){
			for (int i=0;i<w2;i++){
				for (int j=0;j<h2;j++){
					if (j==input2 || i==output2) g.setColor(Color.blue);
					else           g.setColor(new Color(  (100-act2.selectMap.get(obj2)[i][j])/200 ,
						                                  (100+act2.selectMap.get(obj2)[i][j])/200 ,
						                                  (Math.min(10, act1.confidenceMap.get(obj2)[i][j]))/10));
					g.fillRect(10+(int)(j*r2), 590-(int)(i*r2), (int)r2+1, (int)r2+1);	
				}
			}
			for (int i=0;i<act1.links.size();i++){
				g.setColor(act2.objMemory.objectList.get(i));
				g.fillRect(370, i*20+310, (int)(act2.links.get(i).get(obj2)*200), 19);
			}
		}
		g.setColor(Color.black);
		for (int i=0;i<h1;i+=20){
			g.drawLine(10+(int)(i*r1), 590, 10+(int)(i*r1), 590-(int)(w1*r1));
		}
		for (int i=0;i<w1;i+=20){
			g.drawLine(10,590-(int)(i*r1), 10+(int)(h1*r1), 590-(int)(i*r1));
		}
		
		//g.setColor(Color.blue);
		//if (act2.previousObj==obj2) g.drawRect(5,305,590,290);
	}
	
	
	
	/////////////////////////////////////////////
	public void draw3D(Graphics g,Action act1,int obj1,int input1,int output1,
                                  Action act2,int obj2,int input2,int output2){
		
		float h1=act1.height;
		float w1=act1.width;

		float r1=Math.min(580/(2*h1+w1),280/(100+w1));
		
		float h2=act2.height;
		float w2=act2.width;
		
		float r2=Math.min(580/(2*h2+w1),280/(100+w2));
		
        ////////////////////////////////////////////
		// object 1
		for (int i=0;i<w1;i++){
			for (int j=0;j<h1;j++){
				if (j==input1 || i==output1) g.setColor(Color.blue);
				else if (i%20==0 || (j%20==0 && j!=0) ) g.setColor(Color.black);
				else           g.setColor(new Color(  (100-act1.selectMap.get(obj1)[i][j])/200 ,
						                              (100+act1.selectMap.get(obj1)[i][j])/200 ,
						                              (Math.min(10, act1.confidenceMap.get(obj1)[i][j]))/10));
				g.fillRect((int)(50+w1+(2*j*r1)-(i*r1))  ,  (int)(10+(i+50- act1.selectMap.get(obj1)[i][j]/2)*r1), (int)r1+2 , (int)((50+act1.selectMap.get(obj1)[i][j]/2)*r1)+2);
			}
		}
		//g.setColor(Color.blue);
		//if (act1.previousObj==obj1) g.drawRect(5,5,590,290);
		
		
		// object 2
		for (int i=0;i<w2;i++){
			for (int j=0;j<h2;j++){
				if (j==input2 || i==output2) g.setColor(Color.blue);
				else if (i%20==0 || (j%20==0 && j!=0) ) g.setColor(Color.black);
				else           g.setColor(new Color(  (100-act2.selectMap.get(obj2)[i][j])/200 ,
						                              (100+act2.selectMap.get(obj2)[i][j])/200 ,
						                              (Math.min(10, act2.confidenceMap.get(obj2)[i][j]))/10));
				g.fillRect((int)(50+w2+(2*j*r2)-(i*r2))  ,  (int)(310+(i+50- act2.selectMap.get(obj2)[i][j]/2)*r2), (int)r2+2 , (int)((50+act2.selectMap.get(obj2)[i][j]/2)*r2)+2);
			}
		}
		//g.setColor(Color.blue);
		//if (act2.previousObj==obj2) g.drawRect(5,305,590,290);
	}
	
	
	
	
	////////////////////////////////////
	public void paintComponent(Graphics g){
		
		int act1=0;
		int obj1=0;
		int act2=0;
		int obj2=1;
		
		boolean view3D=false;
		
		
		int d1=actList.get(act1).previousInput;
		int c1=actList.get(act1).previousChoice;
		
		int d2=actList.get(act1).previousInput;
		int c2=actList.get(act1).previousChoice;
		
		g.setColor(Color.white);
		g.fillRect(0, 0, 600, 600);
		
		
		
		if (view3D) draw3D(g,actList.get(act1),obj1,d1,c1,
                             actList.get(act2),obj2,d2,c2);
		else        draw2D(g,actList.get(act1),obj1,d1,c1,
			                 actList.get(act2),obj2,d2,c2);

	}

	
}
