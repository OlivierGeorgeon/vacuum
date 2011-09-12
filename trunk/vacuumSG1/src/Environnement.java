
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.*;
import java.awt.geom.Path2D.Float;
import java.util.ArrayList;
import java.awt.Color;
import javax.swing.JPanel;

public class Environnement extends JPanel implements MouseListener{

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;
	
	public Model m_model;
	public int m_h=1;
	public int m_w=1;
	public int m_clicked=0;
	public int m_clickX;
	public int m_clickY;
	
	public int e_x;
	public int e_y;
	
	public float[] ernest_x={ (float) -0.5,0,(float) 0.5};
	public float[] ernest_y={(float) 0.7,(float) -0.8,(float) 0.7};
	
	public float[] fish_x={(float) 0.8,(float) 0.6,(float) 0.4,(float) 0.2,0,(float)-0.2,(float)-0.4,(float)-0.6,(float)-0.8,
			               (float)-0.8,(float)-0.6,(float)-0.4,(float)-0.2,0,(float) 0.2,(float) 0.4,(float) 0.6,(float) 0.8};
	public float[] fish_y={0,(float) 0.4,(float) 0.5,(float) 0.5,(float) 0.5,(float) 0.4,(float) 0.3,0,(float) 0.5,
			               (float)-0.5,0,(float)-0.3,(float)-0.4,(float)-0.5,(float)-0.5,(float)-0.5,(float)-0.4,0};
	
	public float[] leaf_x={ 0, (float) 0.8 , (float) 0.7,(float) 0.2,0};
	public float[] leaf_y={ 0, (float) 0.2 , (float) 0.7,(float) 0.8,0};
	
	
	
	public Environnement(Model model){
		m_model=model;
		m_h=m_model.getHeight();
		m_w=m_model.getWidth();
		addMouseListener(this);
	}
	
	
	public int[] transform_x(float[] polygon_x,float[] polygon_y,float angle_x,float scale_x,int translation_x){
		int[] ret=new int[polygon_x.length];
		for (int i=0;i<polygon_x.length;i++){
			// rotation and scale
			ret[i]=(int)( ( (polygon_x[i]*Math.cos(angle_x)) - (polygon_y[i]*Math.sin(angle_x)) )*scale_x);
			
			// translation
			ret[i]+=translation_x;
		}
		
		return ret;
	}
	
	public int[] transform_y(float[] polygon_x,float[] polygon_y,float angle_y,float scale_y,int translation_y){
		int[] ret=new int[polygon_y.length];
		for (int i=0;i<polygon_y.length;i++){
			// rotation and scale
			ret[i]=(int) (( (polygon_x[i]*Math.sin(angle_y)) + (polygon_y[i]*Math.cos(angle_y)) )*scale_y);
			
			// translation
			ret[i]+=translation_y;
		}
		
		return ret;
	}
	
	
	public void paintComponent(Graphics g){

		Graphics2D g2d = (Graphics2D)g;

		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

		int h=this.getHeight();
		int w=this.getWidth();
		
		int c_w=w/m_w;
		int c_h=h/m_h;
		
		e_x=(int) (m_model.m_x*c_w+c_w/2);
		e_y=(int) (m_model.m_y*c_h+c_h/2);
		
		
		g2d.setColor(Color.white);
		g2d.fillRect(0, 0, w, h);
		
		// draw agent
		m_model.paintAgent((Graphics2D)g.create(),(int) (m_model.m_x*c_w+(c_w/2)),(int) (m_model.m_y*c_h+(c_h/2)),(double)c_w/100,(double)c_h/100);
		
		
		for (int i=0;i<m_w;i++){
			for (int j=0;j<m_h;j++){
				
				// walls
				if (m_model.isWall(i,j)){
					g2d.setColor(m_model.getBackgroundColor(i, j) );
					g2d.fillRect(i*c_w, j*c_h, c_w+1, c_h+1);
					//g2d.setColor(Color.black);
					//g.drawRect(i*c_w, j*c_h, c_w+1, c_h+1);
				}
				
				// fish
				if (m_model.getDirty(i, j)== m_model.DIRTY ){
					g2d.setColor(m_model.getBackgroundColor(i, j) );
					g2d.fillPolygon(transform_x(fish_x,fish_y,0,c_w/2,i*c_w+c_w/2) , 
						          transform_y(fish_x,fish_y,0,c_h/2,j*c_h+c_h/2) , 18);
					g2d.setColor(Color.white);
					g2d.fillOval((int)(i*(w/m_w)+(w/m_w)*0.7), (int)(j*c_h+c_h*0.35), (int)(c_w*0.1), (int)(c_h*0.1));
				}
				
				// leaf
				if (m_model.getDirty(i, j) > m_model.DIRTY )
				{
					g2d.setColor(m_model.getBackgroundColor(i, j) );

					g2d.fillPolygon(transform_x(leaf_x,leaf_y,0,c_w/2,i*c_w+c_w/2) , 
							      transform_y(leaf_x,leaf_y,0,c_h/2,j*c_h+c_h/2) , 4);
					g2d.fillPolygon(transform_x(leaf_x,leaf_y,(float) (Math.PI/2  ),c_w/2,i*c_w+c_w/2) , 
						          transform_y(leaf_x,leaf_y,(float) (Math.PI/2  ),c_h/2,j*c_h+c_h/2) , 4);
					g2d.fillPolygon(transform_x(leaf_x,leaf_y,(float) (Math.PI    ),c_w/2,i*c_w+c_w/2) , 
					              transform_y(leaf_x,leaf_y,(float) (Math.PI    ),c_h/2,j*c_h+c_h/2) , 4);
					g2d.fillPolygon(transform_x(leaf_x,leaf_y,(float) (-Math.PI/2),c_w/2,i*c_w+c_w/2) , 
					              transform_y(leaf_x,leaf_y,(float) (-Math.PI/2),c_h/2,j*c_h+c_h/2) , 4);
				}
				
			}
		}
		
		// draw Ernest
		//g.setColor(Color.black);
		//g.fillPolygon(transform_x(ernest_x,ernest_y,(float) m_model.m_orientationAngle,c_w/2,e_x) , 
		//	          transform_y(ernest_x,ernest_y,(float) m_model.m_orientationAngle,c_h/2,e_y) , 3);
		//g.setColor(Color.red);
		//g.drawOval((int)(e_x-0.4*c_w),(int)(e_y-0.4*c_h),(int)(0.8*c_w),(int)(0.8*c_h));
		
		// draw informations
		drawInformation((Graphics2D)g.create());
		
		// draw dream square
		m_model.paintDream((Graphics2D)g.create(),c_w*(m_w-2)-c_w/2,c_h/2, (double)c_w/100, (double)c_h/100);
		//g2d.setColor(Color.black);
		//g.drawRect(c_w*(m_w-3), 0, c_w, c_h);
		
	}


	public void mouseClicked(MouseEvent e){

		int h=this.getHeight();
		int w=this.getWidth();

		m_clickX= (e.getX() / (int)( (float)w/(float)m_w ));
		m_clickY= (e.getY() / (int)( (float)h/(float)m_h ));
		
		if (e.getButton() == MouseEvent.BUTTON1)
			if (e.isShiftDown()) m_clicked = 4;
			else m_clicked = 1;
		if (e.getButton() == MouseEvent.BUTTON3)
		{
			if (e.isShiftDown()) m_clicked = 3;
			else m_clicked = 2;
		}
	}
	
	public int getClicked()
	{
		int c = m_clicked;
		m_clicked = 0;
		return c;
	}


	@Override
	public void mouseEntered(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void mouseExited(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void mousePressed(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void mouseReleased(MouseEvent arg0) {
		// TODO Auto-generated method stub
		
	}
	
	/**
	 * Draw the information square.
	 */
	private void drawInformation(Graphics2D g2d){
		
		int h=this.getHeight();
		int w=this.getWidth();
		
		int c_w=w/m_w;
		int c_h=h/m_h;
		
		String counter = m_model.getCounter() + ""; 
		
		Font font = new Font("Dialog", Font.BOLD, 10);
		g2d.setFont(font);
		
		FontMetrics fm = getFontMetrics(font);

		int width = fm.stringWidth(counter);
		
		g2d.setColor(new Color(200, 255, 200));		
		g2d.drawString(counter, m_w*c_w - c_w/10 - width, c_h/2+5);	
	}
	
}
