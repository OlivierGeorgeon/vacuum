package agent;
import java.awt.Graphics;
import java.awt.Color;
import java.util.ArrayList;

import javax.swing.JPanel;
import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;


/**
 * Panel used to display environment on the point of view of the agent
 * @author simon
 */
public class EyeDisplay extends JPanel {

		/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
		private static boolean complete=true;
		private static boolean view=true;
		private static boolean attractness=false;
		
		int left,right;
		
		public InternalView eye;
		
		//InternalMap map;
		//int max;
		
		//double[] attract=new double[180];
		
        public EyeDisplay(InternalView m_eye){
        	/*for (int i=0;i<180;i++){
        		attract[i]=0;
        	}*/
        	//map=m;
    		if (complete){
    			left=0;
    			right=720;
    		}
    		else{
    			left=180;
    			right=540;
    		}
    		eye=m_eye;
        }
        
        public void setEye(InternalView e){
        	eye=e;
        }
        
	
        public void paintComponent(Graphics g){
        	/*if (view){
        		// visual
        			g.setColor(Color.BLUE);
        			g.fillRect(0,0,720,150);
                	
        			g.setColor(Color.orange);
        			g.fillRect(0,150,720,150);
        			
        			if (!complete){
        				g.setColor(Color.black);
            			g.fillRect(0,0,180,300);
            			g.fillRect(540,0,720,300);
        			}
        			for (int i=left;i<right;i++){
        				double d=100/ (Math.max(0.1,eye.retine[i/2]/10.));
        				d=Math.min(150,d);
        				g.setColor(eye.colorMap[i/2]);
        				g.drawLine(i  ,(int)( 150- d ),i  ,(int)( 150+ d  ));
        				
        				g.setColor(Color.black);
        				if (d!=150){
        					g.drawLine(i, (int)( 150- d ), i, (int)( 150- d ));
        					g.drawLine(i, (int)( 150+ d ), i, (int)( 150+ d ));
        				}
        				if ((eye.corner[i/2]==1)){
        					g.drawLine((int)(i/2)*2  ,(int)( 150- d ),(int)(i/2)*2  ,(int)( 150+ d  ));
        				}
        				if ((eye.corner[i/2]==2)){
        					g.drawLine((int)(i/2)*2+1  ,(int)( 150- d ),(int)(i/2)*2+1  ,(int)( 150+ d  ));
        				}
        			}

        		// tactile
        			g.setColor(Color.magenta);
                	g.fillRect(0,300,720,150);
                	
                	g.setColor(Color.cyan);
                	g.fillRect(0,450,720,150);
        			for (int i=0;i<720;i++){
                		if (eye.tactileMap[i/2]==1) g.setColor(new Color(150,150,150));
                		if (eye.tactileMap[i/2]==2) g.setColor(new Color(100,100,100));
                		if (eye.tactileMap[i/2]==3) g.setColor(new Color(50,50,50));
                		if (eye.tactileMap[i/2]==4) g.setColor(new Color(100,50,50));
                		
                		if (complete || eye.tactile[i/2]<=15){
                			double d=100/ (Math.max(0.1,eye.tactile[i/2]/10.));
                			d=Math.min(150,d);
                			g.drawLine(i  ,(int)( 450- d ),i  ,(int)( 450+ d ));
                		
                			g.setColor(Color.black);
                			if (d!=150){
                				g.drawLine(i, (int)( 450- d ), i, (int)( 450- d ));
                				g.drawLine(i, (int)( 450+ d ), i, (int)( 450+ d ));
                			}
                  	  		if ((eye.corner2[i/2]==1))
                  	  			g.drawLine((int)(i/2)*2  ,(int)( 450- d ),(int)(i/2)*2  ,(int)( 450+ d ));
                  	  		if ((eye.corner2[i/2]==2))
                  	  			g.drawLine((int)(i/2)*2+1,(int)( 450- d ),(int)(i/2)*2+1,(int)( 450+ d ));
                		}
                		else{
                			g.setColor(Color.black);
                			g.drawLine(i  ,(int)( 450- 100/ (Math.max(0.1,1.5)) ),i  ,(int)( 450+ 100/ (Math.max(0.1,1.5))  ));
                		}
                    }
        	}
        	else{
                for (int i=0;i<720;i++){
                	g.setColor(eye.colorMap[i/4]);
              	  	g.drawLine(i  ,300,i  ,(int)eye.retine[i/4]+100);
                }
        	}
        	
        	g.setColor(Color.BLUE);
        	g.drawLine(0, 150, 720, 150);
        	g.drawLine(0, 450, 720, 450);
        	/**/
        	
        	/*
        	g.setColor(Color.RED);
        	if (attractness){
        		for (int i=0;i<179;i++){
        			g.drawLine(i*4, (int)(150-5*map.map[i]), 4*i+4, (int)(150-5*map.map[i+1]));
        		}
        		g.fillOval((map.imax*4)-4, (int) (150-5*map.map[map.imax])-4, 8, 8);
        	
        	}*/
        	
        	Vector3f localSpeed=new Vector3f();
        	
        	g.setColor(Color.red);
        	g.fillOval(300,300,5,5);
        	int size=eye.cornerPoints.size();
        	for (int i=0;i<size;i++){
        		g.setColor(Color.blue);
        		g.fillOval(300+(int)(eye.cornerPoints.get(i).x*4),300+(int)(eye.cornerPoints.get(i).y*4),5,5);
        		
        		g.setColor(Color.red);
        		
        		localSpeed.scale(0);
        		localSpeed.x=(float) (eye.retine[(int)eye.cornerPoints.get(i).z]/10)*eye.speedR.z;
        		
        		Matrix3f rot = new Matrix3f();
        		rot.rotZ((float) ( -(eye.cornerPoints.get(i).z*Math.PI/180)));
        		rot.transform(localSpeed, localSpeed);
        		
        		
        		localSpeed.add(eye.speedT);
        		
        		
        		
        		g.drawLine(300+(int)(eye.cornerPoints.get(i).x*4+2),
        				   300+(int)(eye.cornerPoints.get(i).y*4+2),
        				   300+(int)(eye.cornerPoints.get(i).x*4 - localSpeed.x*200+2),
        				   300+(int)(eye.cornerPoints.get(i).y*4 + localSpeed.y*200+2));
        	}
        	  
        }   
        
}
