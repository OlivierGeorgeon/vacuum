package paused;
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

/**
 * Panel used to display Action matrix values
 * @author simon
 */
public class InternalStatesPanel extends JPanel{

	private ArrayList<Action> actList;
	private int distance;
	
	public InternalStatesPanel(ArrayList<Action> aList){
		actList=aList;
	}

	
	/**
	 * draw matrix for each action and objects
	 * @param g Graphic object
	 */
	public void draw2D(Graphics g){
		
		int xoffset=102;
		int yoffset=102;
		
		for (int k=0;k<actList.size();k++){
			
			float h1=actList.get(k).height/5;
			float w1=actList.get(k).width/5;

			float r1=Math.min(100/h1,100/w1);
			
			float h2=actList.get(k).height/5;
			float w2=actList.get(k).width/5;
			
			float r2=Math.min(100/h2,100/w2);
			
			// draw action name
			g.setColor(Color.black);
			g.drawString(actList.get(k).name, 20+k*xoffset, 10);

			int input=actList.get(k).previousInput/5;
			int output=actList.get(k).previousChoice/5;
			
			g.setColor(Color.black);
			g.drawLine(5, 18, 5+actList.size()*xoffset+250, 18);
			
			for (int l=0;l<actList.get(k).selectMap.size();l++){
				
				g.setColor(Color.black);
				if (l<5) g.drawLine(5, (l+1)*yoffset+18, 5+actList.size()*xoffset+250, (l+1)*yoffset+18);
				else     g.drawLine(5+actList.size()*xoffset+260, (l-5)*yoffset+18, 20+actList.size()*xoffset+255+actList.size()*xoffset+260, (l-5)*yoffset+18);
				
				// draw each object
				for (int i=0;i<w1;i++){
					for (int j=0;j<h1;j++){
						if (j==input || i==output) g.setColor(Color.blue);
						else           g.setColor(new Color(  (100-actList.get(k).selectMap.get(l)[i][j])/200 ,
															  (100+actList.get(k).selectMap.get(l)[i][j])/200 ,
							                                  (Math.min(10, actList.get(k).confidenceMap.get(l)[i][j]))/10));
						
						if (l<5) g.fillRect(k*xoffset+20+(int)(j*r1), l*yoffset+15+(int)(yoffset-i*r1), (int)r1+1, (int)r1+1);
						else     g.fillRect(k*xoffset+20+(int)(j*r1) + actList.size()*xoffset+260, (l-5)*yoffset+15+(int)(yoffset-i*r1), (int)r1+1, (int)r1+1);
					}
				}
				
				// draw color of the object
				g.setColor( actList.get(0).objMemory.objectList.get(l));
				if (l<5) g.fillRect(25+xoffset*actList.size(), 25+l*yoffset, 10, yoffset-15);
				else     g.fillRect(25+xoffset*actList.size()+ actList.size()*xoffset+260, 25+(l-5)*yoffset, 10, yoffset-15);
				
				// display distance
				if (k==0 && actList.size()>0 && actList.get(0).distances.size()>0){
					for (int m=0;m<actList.get(0).objMemory.objectList.size();m++){
						g.setColor( actList.get(0).objMemory.objectList.get(m));
						if (l<5) g.fillRect((int) (25+xoffset*actList.size()+ actList.get(0).distances.get(l).get(m)/2), 25+l*yoffset+10*m, 10,10);
						else     g.fillRect((int) (25+xoffset*actList.size()+ actList.get(0).distances.get(l).get(m)/2)+ actList.size()*xoffset+260, 25+(l-5)*yoffset+10*m, 10,10);
					}
				}
				
			}
		}
	}
	
	
	
	/**
	 * draw two matrix, chosen by the user, in 3 dimensions
	 * @param g         Graphic object
	 * @param act1		act of the first matrix
	 * @param obj1		object of the first matrix
	 * @param input1	input value of the first matrix
	 * @param output1	output value of the first matrix
	 * @param act2		act of the second matrix
	 * @param obj2		object of the second matrix
	 * @param input2	input value of the second matrix
	 * @param output2	output value of the second matrix
	 */
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
		if (act1.selectMap.size()>obj1){
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
		g.setColor(Color.blue);
		if (act1.previousObj==obj1) g.drawRect(5,5,590,290);
		}
		
		// object 2
		if (act2.selectMap.size()>obj2){
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
		g.setColor(Color.blue);
		if (act2.previousObj==obj2) g.drawRect(5,305,590,290);
		}
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
		g.fillRect(0, 0, 1200, 600);
		
		
		
		if (view3D) draw3D(g,actList.get(act1),obj1,d1,c1,
                             actList.get(act2),obj2,d2,c2);
		else        draw2D(g);

	}

	
}
