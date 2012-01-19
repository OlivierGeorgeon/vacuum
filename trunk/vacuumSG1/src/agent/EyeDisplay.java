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
		private static boolean viewORsegments=false;
		
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
        	if (viewORsegments){
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
        			
        			g.setColor(Color.black);
        			for (int i=0;i<eye.cornerPoints.size();i++){
        				double d=100/ (Math.max(0.1,eye.cornerPoints.get(i).distance));
        				d=Math.min(150,d);
        				g.drawLine((int)(eye.cornerPoints.get(i).angle)*2  ,(int)( 150- d ),(int)(eye.cornerPoints.get(i).angle)*2  ,(int)( 150+ d  ));
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
        			
        			g.setColor(Color.BLUE);
                	g.drawLine(0, 150, 720, 150);
                	g.drawLine(0, 450, 720, 450);
        	}
        	
        	
        	else{
        		
        		boolean wallPoints=false;				// draw non corner points (if created)
        		boolean pointColors=true;				// draw left and right color of points
        		boolean pointSpeed=true;               // draw speed vector of corner points
        		boolean drawSegments=true;				// draw segments
        		boolean segmentSpeed=false;              // draw segment speed vectors
        		
        		Vector3f localSpeed=new Vector3f();
        		g.setColor(Color.red);
        		g.fillOval(300,300,5,5);
        		int size=eye.cornerPoints.size();


        	
        		
        		
        		int scale=40;
        		for (int i=0;i<size;i++){
 
        			// draw non corner points
            		if (wallPoints){
            			if (eye.cornerPoints.get(i).type==10){
            				g.setColor(Color.green);
            				g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
            			}
            		}
        			
            		// draw left and right points' colors
        			if (pointColors){
        				if (eye.cornerPoints.get(i).type!=10){
        					g.setColor(eye.cornerPoints.get(i).leftColor);
        					if (!eye.cornerPoints.get(i).leftColor.equals(Color.black)) g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-6),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
        					g.setColor(eye.cornerPoints.get(i).rightColor);
        					if (!eye.cornerPoints.get(i).rightColor.equals(Color.black))g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale+2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
        				}
        			}
        			
        			// draw points
        			if (eye.cornerPoints.get(i).type==0){
        				g.setColor(Color.blue);
        				g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
        				
        				// draw speed vector of corner points
        				if (pointSpeed){
        					g.setColor(Color.red);
        					g.drawLine(300+(int)(eye.cornerPoints.get(i).position.x*scale+2),
        						       300-(int)(eye.cornerPoints.get(i).position.y*scale-2),
        						       300+(int)(eye.cornerPoints.get(i).position.x*scale + eye.cornerPoints.get(i).speed.x*200+2),
        						       300-(int)(eye.cornerPoints.get(i).position.y*scale + eye.cornerPoints.get(i).speed.y*200-2));
        				}
                     }
                     else{           
                    	 if (eye.cornerPoints.get(i).type==1 || eye.cornerPoints.get(i).type==2){
                    		 g.setColor(Color.red);
                    		 g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
                    	 }
                    	 if (eye.cornerPoints.get(i).type==3){
                    		 g.setColor(Color.yellow);
                    		 g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
                    	 }
                    	 
                    	 if (eye.cornerPoints.get(i).type==4){
                    		 g.setColor(Color.cyan);
                    		 g.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*scale-2),300-(int)(eye.cornerPoints.get(i).position.y*scale+2),5,5);
                    	 }
                     }
        		}
            
        		if (drawSegments){
        			for (int i=0;i<eye.segments.size();i++){
                   	 g.setColor(new Color(eye.segments.get(i).getValue()));
                	 g.drawLine(300+(int)(eye.segments.get(i).getFirstPosition().x*scale), 300-(int)(eye.segments.get(i).getFirstPosition().y*scale),
                			    300+(int)(eye.segments.get(i).getSecondPosition().x*scale), 300-(int)(eye.segments.get(i).getSecondPosition().y*scale));
        			}
        		}
             
        		if (segmentSpeed){
        			g.setColor(Color.red);
                    for (int i=0;i<eye.segments.size();i++){
                   	 g.drawLine(300+(int)eye.segments.get(i).getPosition().x*scale,
                   			    300-(int)eye.segments.get(i).getPosition().y*scale,
                   			    300+(int)(eye.segments.get(i).getPosition().x*scale+ eye.segments.get(i).getSpeed().x*500),
                   			    300-(int)(eye.segments.get(i).getPosition().y*scale+ eye.segments.get(i).getSpeed().y*500) );
        			}
        		}

        	}
        }   
        
}

