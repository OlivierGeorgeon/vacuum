package agent;
import java.awt.Graphics;
import java.awt.Color;
import javax.swing.JPanel;


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
	
		double[] retine;
		double[] tactile;
		Color[] colorMap;
		int[] tactileMap;
		int[] corner;
		int[] corner2;
		
		int left,right;
		
		//InternalMap map;
		//int max;
		
		//double[] attract=new double[180];
		
        public EyeDisplay(){
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
        }
        
        /**
         * update the environment characteristics
         * @param r    Distance vector
         * @param cm   Color vector
         * @param cor  Position of the corners of squares
         */
        public void updateRetine(double[] r,Color[] cm,int[] cor,double[] rt,int[] cmt,int[] cort){
        	retine=r;
        	colorMap=cm;
        	corner=cor;
        	
        	tactile=rt;
        	tactileMap=cmt;
        	corner2=cort;
        }
	
        public void paintComponent(Graphics g){
        	if (view){
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
        				double d=100/ (Math.max(0.1,retine[i/2]/10.));
        				d=Math.min(150,d);
        				g.setColor(colorMap[i/2]);
        				g.drawLine(i  ,(int)( 150- d ),i  ,(int)( 150+ d  ));
        				
        				g.setColor(Color.black);
        				if (d!=150){
        					g.drawLine(i, (int)( 150- d ), i, (int)( 150- d ));
        					g.drawLine(i, (int)( 150+ d ), i, (int)( 150+ d ));
        				}
        				if ((corner[i/2]==1)){
        					g.drawLine((int)(i/2)*2  ,(int)( 150- d ),(int)(i/2)*2  ,(int)( 150+ d  ));
        				}
        				if ((corner[i/2]==2)){
        					g.drawLine((int)(i/2)*2+1  ,(int)( 150- d ),(int)(i/2)*2+1  ,(int)( 150+ d  ));
        				}
        			}

        		// tactile
        			g.setColor(Color.magenta);
                	g.fillRect(0,300,720,150);
                	
                	g.setColor(Color.cyan);
                	g.fillRect(0,450,720,150);
        			for (int i=0;i<720;i++){
                		if (tactileMap[i/2]==1) g.setColor(new Color(150,150,150));
                		if (tactileMap[i/2]==2) g.setColor(new Color(100,100,100));
                		if (tactileMap[i/2]==3) g.setColor(new Color(50,50,50));
                		if (tactileMap[i/2]==4) g.setColor(new Color(100,50,50));
                		
                		if (complete || tactile[i/2]<=15){
                			double d=100/ (Math.max(0.1,tactile[i/2]/10.));
                			d=Math.min(150,d);
                			g.drawLine(i  ,(int)( 450- d ),i  ,(int)( 450+ d ));
                		
                			g.setColor(Color.black);
                			if (d!=150){
                				g.drawLine(i, (int)( 450- d ), i, (int)( 450- d ));
                				g.drawLine(i, (int)( 450+ d ), i, (int)( 450+ d ));
                			}
                  	  		if ((corner2[i/2]==1))
                  	  			g.drawLine((int)(i/2)*2  ,(int)( 450- d ),(int)(i/2)*2  ,(int)( 450+ d ));
                  	  		if ((corner2[i/2]==2))
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
                	g.setColor(colorMap[i/4]);
              	  	g.drawLine(i  ,300,i  ,(int)retine[i/4]+100);
                }
        	}
        	
        	g.setColor(Color.BLUE);
        	g.drawLine(0, 150, 720, 150);
        	g.drawLine(0, 450, 720, 450);
        	
        	/*
        	g.setColor(Color.RED);
        	if (attractness){
        		for (int i=0;i<179;i++){
        			g.drawLine(i*4, (int)(150-5*map.map[i]), 4*i+4, (int)(150-5*map.map[i+1]));
        		}
        		g.fillOval((map.imax*4)-4, (int) (150-5*map.map[map.imax])-4, 8, 8);
        	
        	}*/
        	  
        }   
        
}

