


import java.awt.Graphics;
import java.awt.Color;


import javax.swing.JPanel;
 
public class EyeDisplay extends JPanel {
 
		/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
		private static boolean view=true;
		private static boolean attractness=true;
	
		double[] retine;
		Color[] colorMap;
		int[] corner;
		InternalMap map;
		int max;
		
		double[] attract=new double[180];
		
        public EyeDisplay(InternalMap m){
        	for (int i=0;i<180;i++){
        		attract[i]=0;
        	}
        	map=m;
        }
        
        public void updateRetine(double[] r,Color[] cm,int[] cor){
        	retine=r;
        	colorMap=cm;
        	corner=cor;
        }
	
        public void paintComponent(Graphics g){

        		
        	if (view){
        		g.setColor(Color.BLUE);
            	g.fillRect(0,0,720,150);
            	
            	g.setColor(Color.orange);
            	g.fillRect(0,150,720,150);
            	
            	for (int i=0;i<720;i++){
            		//System.out.println(i/4+" "+colorMap.length);
            		g.setColor(colorMap[i/4]);
              	  	//g.drawLine(i  ,(int)(retine[i/4]/2+80),i  ,300-(int)(retine[i/4]/2)-80);
            		g.drawLine(i  ,(int)( 150- 100/ (Math.max(0.1,retine[i/4]/10.)) ),i  ,(int)( 150+ 100/ (Math.max(0.1,retine[i/4])/10.)  ));
            		
            		g.setColor(Color.black);
            		g.drawLine(i, (int)( 150- 100/ (Math.max(0.1,retine[i/4]/10.)) ), i, (int)( 150- 100/ (Math.max(0.1,retine[i/4]/10.)) ));
            		g.drawLine(i, (int)( 150+ 100/ (Math.max(0.1,retine[i/4]/10.)) ), i, (int)( 150+ 100/ (Math.max(0.1,retine[i/4]/10.)) ));
            		
              	  	if ((corner[i/4]==1)){
              	  		//g.drawLine((int)(i/4)*4  ,(int)(retine[i/4]/2+80),(int)(i/4)*4  ,300-(int)(retine[i/4]/2)-80);
              	  		g.drawLine((int)(i/4)*4  ,(int)( 150- 100/ (Math.max(0.1,retine[i/4]/10.)) ),(int)(i/4)*4  ,(int)( 150+ 100/ (Math.max(0.1,retine[i/4])/10.)  ));
              	  	}
              	  	if ((corner[i/4]==2)){
            	  		//g.drawLine((int)(i/4)*4  ,(int)(retine[i/4]/2+80),(int)(i/4)*4  ,300-(int)(retine[i/4]/2)-80);
            	  		g.drawLine((int)(i/4)*4+3  ,(int)( 150- 100/ (Math.max(0.1,retine[i/4]/10.)) ),(int)(i/4)*4+3  ,(int)( 150+ 100/ (Math.max(0.1,retine[i/4])/10.)  ));
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
        	
        	g.setColor(Color.RED);
        	if (attractness){
        		for (int i=0;i<179;i++){
        			g.drawLine(i*4, (int)(150-5*map.map[i]), 4*i+4, (int)(150-5*map.map[i+1]));
        		}
        		g.fillOval((map.imax*4)-4, (int) (150-5*map.map[map.imax])-4, 8, 8);
        	
        	}
        	  
        }   
        
}

