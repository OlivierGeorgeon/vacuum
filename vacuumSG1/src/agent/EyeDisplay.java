package agent;
import java.awt.BasicStroke;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.util.ArrayList;

import javax.swing.JPanel;
import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;

import spas.IPlace;
import spas.Spas;

import ernest.Ernest;


/**
 * Panel used to display environment on the point of view of the agent
 * @author simon
 */
public class EyeDisplay extends JPanel {

	/** The radius of the display area in grid units. */
	public final static int RADIUS = 10;
	
	/** The number of pixels per grid units. */
	public final static int SCALE = 32; 
	
	/** The radius of the display area in pixels. */
	//public final static int RADIUS_SCALE = 300; 
	public final static int WIDTH = 300;
	public final static int HEIGHT = 250;
	
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
    

    public void paintComponent(Graphics g)
    {	
		Graphics2D g2d = (Graphics2D)g;
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);
    	
        // display the agent.
        AffineTransform ref = g2d.getTransform();
        AffineTransform orientation = new AffineTransform();
        orientation.translate(WIDTH, HEIGHT);
        orientation.rotate(Math.PI/2);
        orientation.scale(SCALE / 100f, SCALE / 100f);
        g2d.transform(orientation);
		g2d.setColor(Color.gray);
		if (eye.m_model.getCuddle())
			g2d.setColor(Color.PINK);
		if (eye.m_model.getEat())
			g2d.setColor(Color.YELLOW);
        g2d.fill(Ernest110Model.shape(eye.getID()));
        g2d.setTransform(ref);

        // Display counter
		String counter = eye.getCounter() + ""; 
		Font font = new Font("Dialog", Font.BOLD, 18);
		g2d.setFont(font);
		FontMetrics fm = getFontMetrics(font);
		int width = fm.stringWidth(counter);
		g2d.setColor(Color.GRAY);		
		g2d.drawString(counter, 2 * WIDTH - 30 - width, 30);	
		
        // Points        
        //Arc2D.Double pie = new Arc2D.Double(-SCALE /8, -SCALE /8, SCALE/4, SCALE/4, 0, 180, Arc2D.PIE);
        Arc2D.Double pie = new Arc2D.Double(-SCALE /4, -SCALE /4, SCALE/2, SCALE/2, 0, 180, Arc2D.PIE);

    	if (viewORsegments)
    	{
    		// visual
    			g2d.setColor(Color.BLUE);
    			g2d.fillRect(0,0,720,150);
            	
    			g2d.setColor(Color.orange);
    			g2d.fillRect(0,150,720,150);
    			
    			if (!complete){
    				g2d.setColor(Color.black);
        			g2d.fillRect(0,0,180,300);
        			g2d.fillRect(540,0,720,300);
    			}
    			for (int i=left;i<right;i++){
    				double d=100/ (Math.max(0.1,eye.retine[i/2]/10.));
    				d=Math.min(150,d);
    				g2d.setColor(eye.colorMap[i/2]);
    				g2d.drawLine(i  ,(int)( 150- d ),i  ,(int)( 150+ d  ));
    				
    				g2d.setColor(Color.black);
    				if (d!=150){
    					g2d.drawLine(i, (int)( 150- d ), i, (int)( 150- d ));
    					g2d.drawLine(i, (int)( 150+ d ), i, (int)( 150+ d ));
    				}
    				if ((eye.corner[i/2]==1)){
    					g2d.drawLine((int)(i/2)*2  ,(int)( 150- d ),(int)(i/2)*2  ,(int)( 150+ d  ));
    				}
    				if ((eye.corner[i/2]==2)){
    					g2d.drawLine((int)(i/2)*2+1  ,(int)( 150- d ),(int)(i/2)*2+1  ,(int)( 150+ d  ));
    				}
    			}
    			
    			g2d.setColor(Color.black);
    			for (int i=0;i<eye.cornerPoints.size();i++){
    				double d=100/ (Math.max(0.1,eye.cornerPoints.get(i).distance));
    				d=Math.min(150,d);
    				g2d.drawLine((int)(eye.cornerPoints.get(i).angle)*2  ,(int)( 150- d ),(int)(eye.cornerPoints.get(i).angle)*2  ,(int)( 150+ d  ));
    			}

    		// tactile
    			g2d.setColor(Color.magenta);
            	g2d.fillRect(0,300,720,150);
            	
            	g2d.setColor(Color.cyan);
            	g2d.fillRect(0,450,720,150);
    			for (int i=0;i<720;i++){
            		if (eye.tactileMap[i/2]==1) g2d.setColor(new Color(150,150,150));
            		if (eye.tactileMap[i/2]==2) g2d.setColor(new Color(100,100,100));
            		if (eye.tactileMap[i/2]==3) g2d.setColor(new Color(50,50,50));
            		if (eye.tactileMap[i/2]==4) g2d.setColor(new Color(100,50,50));
            		
            		if (complete || eye.tactile[i/2]<=15){
            			double d=100/ (Math.max(0.1,eye.tactile[i/2]/10.));
            			d=Math.min(150,d);
            			g2d.drawLine(i  ,(int)( 450- d ),i  ,(int)( 450+ d ));
            		
            			g2d.setColor(Color.black);
            			if (d!=150){
            				g2d.drawLine(i, (int)( 450- d ), i, (int)( 450- d ));
            				g2d.drawLine(i, (int)( 450+ d ), i, (int)( 450+ d ));
            			}
              	  		if ((eye.corner2[i/2]==1))
              	  			g2d.drawLine((int)(i/2)*2  ,(int)( 450- d ),(int)(i/2)*2  ,(int)( 450+ d ));
              	  		if ((eye.corner2[i/2]==2))
              	  			g2d.drawLine((int)(i/2)*2+1,(int)( 450- d ),(int)(i/2)*2+1,(int)( 450+ d ));
            		}
            		else{
            			g2d.setColor(Color.black);
            			g2d.drawLine(i  ,(int)( 450- 100/ (Math.max(0.1,1.5)) ),i  ,(int)( 450+ 100/ (Math.max(0.1,1.5))  ));
            		}
                }
    			
    			g2d.setColor(Color.BLUE);
            	g2d.drawLine(0, 150, 720, 150);
            	g2d.drawLine(0, 450, 720, 450);
    	}
    	else
    	{
    		boolean wallPoints=false;				// draw non corner points (if created)
    		boolean pointColors=true;				// draw left and right color of points
    		boolean pointSpeed=true;               // draw speed vector of corner points
    		boolean drawSegments=true;				// draw segments
    		boolean segmentSpeed=false;              // draw segment speed vectors
        		
        	Vector3f localSpeed=new Vector3f();
        		
        	// Draw the agent
        	//g2d.setColor(Color.red);
        	//g2d.fillOval(300,300,5,5);
        		
        	if (eye.lock)
        	{
	        	for (int i=0;i<eye.cornerPoints.size();i++){
	        		
	        		// draw non corner points
	        		if (wallPoints){
	        			if (eye.cornerPoints.get(i).type==10){
	        				g2d.setColor(Color.green);
	        				g2d.fillOval(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE-2),HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	        			}
	        		}
	        		// draw left and right points' colors
	    			if (pointColors){
	    				if (eye.cornerPoints.get(i).type!=10){
	    					g2d.setColor(eye.cornerPoints.get(i).leftColor);
	    					if (!eye.cornerPoints.get(i).leftColor.equals(Color.black)) 
	    					{
	    						//g2d.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*SCALE-6),300-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	    				        ref = g2d.getTransform();
	    				        orientation = new AffineTransform();
	    				        orientation.translate(WIDTH + (eye.cornerPoints.get(i).position.x) * SCALE, HEIGHT - (eye.cornerPoints.get(i).position.y) * SCALE);
	    				        orientation.rotate(- eye.cornerPoints.get(i).getAngle());
	    				        g2d.transform(orientation);
	    				        g2d.fill(pie);
	    				        g2d.setTransform(ref);
	    					}
	    					g2d.setColor(eye.cornerPoints.get(i).rightColor);
	    					if (!eye.cornerPoints.get(i).rightColor.equals(Color.black))
	    					{
	    						//g2d.fillOval(300+(int)(eye.cornerPoints.get(i).position.x*SCALE+2),300-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	    				        ref = g2d.getTransform();
	    				        orientation = new AffineTransform();
	    				        orientation.translate(WIDTH + (eye.cornerPoints.get(i).position.x) * SCALE, HEIGHT - (eye.cornerPoints.get(i).position.y) * SCALE);
	    				        orientation.rotate( - eye.cornerPoints.get(i).getAngle() + Math.PI);
	    				        g2d.transform(orientation);
	    				        g2d.fill(pie);
	    				        g2d.setTransform(ref);
	    					}
	    				}
	    			}
	    			
	    			// draw points
	    			if (eye.cornerPoints.get(i).type<=0){
	    				g2d.setColor(Color.blue);
	    				g2d.fillOval(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE-2),HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	    				
	    				// draw speed vector of corner points
	    				if (pointSpeed){
	    	    			g2d.setStroke(new BasicStroke(SCALE / 20f));
	    					g2d.setColor(Color.red);
	    					g2d.drawLine(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE+2),
	    						       HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE-2),
	    						       WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE + eye.cornerPoints.get(i).speed.x*200+2),
	    						       HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE + eye.cornerPoints.get(i).speed.y*200-2));
	    				}
	                 }
	                 else{           
	                	 if (eye.cornerPoints.get(i).type==1 || eye.cornerPoints.get(i).type==2){
	                		 g2d.setColor(Color.red);
	                		 g2d.fillOval(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE-2),HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	                	 }
	                	 if (eye.cornerPoints.get(i).type==3){
	                		 g2d.setColor(Color.yellow);
	                		 g2d.fillOval(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE-2),HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	                	 }
	                	 
	                	 if (eye.cornerPoints.get(i).type==4){
	                		 g2d.setColor(Color.cyan);
	                		 g2d.fillOval(WIDTH+(int)(eye.cornerPoints.get(i).position.x*SCALE-2),HEIGHT-(int)(eye.cornerPoints.get(i).position.y*SCALE+2),5,5);
	                	 }
	                 }
	    		}
	        
	    		if (drawSegments){
	    			g2d.setStroke(new BasicStroke(SCALE / 10f));
	    			for (int i=0;i<eye.segments.size();i++){
	               	 g2d.setColor(new Color(eye.segments.get(i).getValue()));
	            	 g2d.drawLine(WIDTH+(int)(eye.segments.get(i).getFirstPosition().x*SCALE), HEIGHT-(int)(eye.segments.get(i).getFirstPosition().y*SCALE),
	            			    WIDTH+(int)(eye.segments.get(i).getSecondPosition().x*SCALE), HEIGHT-(int)(eye.segments.get(i).getSecondPosition().y*SCALE));
	    			}
	    		}
	         
	    		if (segmentSpeed){
	    			//g2d.setStroke(new BasicStroke(SCALE / 5f));
	    			g2d.setColor(Color.red);
	                for (int i=0;i<eye.segments.size();i++){
	               	 g2d.drawLine(WIDTH+(int)eye.segments.get(i).getPosition().x*SCALE,
	               			    HEIGHT-(int)eye.segments.get(i).getPosition().y*SCALE,
	               			    WIDTH+(int)(eye.segments.get(i).getPosition().x*SCALE+ eye.segments.get(i).getSpeed().x*500),
	               			    HEIGHT-(int)(eye.segments.get(i).getPosition().y*SCALE+ eye.segments.get(i).getSpeed().y*500) );
	    			}
	    		}
        	}
    	}    	
    }   
}

