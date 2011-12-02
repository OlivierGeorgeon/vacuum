
import javax.vecmath.Vector3f;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;

import ernest.*;
import tracing.*;
import utils.Pair;


/**************************************
 * A Model for Ernest 10.4
 * Ernest gives impulsions for moving forward and turning
 * @author ogeorgeon
 **************************************/
public class Ernest104Model extends ErnestModel 
{
	protected static final int ORIENTATION_UP_RIGHT    = 5; 
	protected static final int ORIENTATION_DOWN_RIGHT  = 15; 
	protected static final int ORIENTATION_DOWN_LEFT   = 25; 
	protected static final int ORIENTATION_UP_LEFT     = 35; 
	public static Color UNANIMATED_COLOR = Color.GRAY;
	
	private double m_rotation_speed = Math.PI/Ernest.RESOLUTION_RETINA;
	
	public float m_If,m_Ir;   				// impulsion counters
	public float distance=0;
	public float angle=0;
	public boolean tempo=true;
	public float m_v;                       // linear speed
	public float m_theta;					// angular speed

	/**
	 * Initialize the agent in the grid
	 */
	public void init(String f) throws Exception
	{
		// Initialize the model
		super.init(f);
		setEyeAngle(Math.PI/4);
		setOrientationStep(5);

		setChanged();
		notifyObservers2();					
	}

	/**
	 * @return The version of the Ernest model
	 */
	public String getVersion()
	{
		return "Ernest 10.4";
	}
	
	/**
	 * Initialize the Ernest agent.
	 */
	public void initErnest()
	{
		m_ernest = new Ernest();
		m_sensorymotorSystem = new Visual100SensorymotorSystem();
		//m_tracer = new XMLTracer("trace.xml");
		//m_tracer = new XMLStreamTracer("http://vm.liris.cnrs.fr:34080/abstract/light/php/stream/","fjSmkmyvAKgByfDAfXUYGjAJLzrWrf");
		//m_tracer = new XMLStreamTracer("http://liristyh.univ-lyon1.fr/alite/php/stream/","ewPmHfhGycqtOYLBNBKOMLalAPmQdj");
		//m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","pDCJHmOykTgkgyZbKVtHFEtS-PujoS");
		m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","h-yXVWrEwtYclxuPQUlmTOXprcFzol");
		
		
		// Initialize the Ernest === 
		
		m_ernest.setParameters(5, 4);
		m_ernest.setTracer(m_tracer);
		m_ernest.setSensorymotorSystem(m_sensorymotorSystem);

		// Ernest's inborn primitive interactions
		
		m_ernest.addInteraction(">", " ",   20); // Move
		//m_ernest.addInteraction(">", "w", -100); // Bump 
		
		m_ernest.addInteraction("^", " ",  -10); // Left toward empty
		//m_ernest.addInteraction("^", "w",  -20); // Left toward wall

		m_ernest.addInteraction("v", " ",  -10); // Right toward empty
		//m_ernest.addInteraction("v", "w",  -20); // Right toward wall
		
		System.out.println("Ernest initialized") ;
	}
	
	/**
	 * Initialize the agent's parameters
	 */
	protected void initAgent()
	{
	}

	/**
	 * Run Ernest one step
	 */
	public String stepErnest(boolean status)
	{
		if (m_tracer != null)
			m_tracer.startNewEvent(m_counter);

		// See the environment
		// 12 visual pixels * 8 visual info + 1 miscelaneous + 3 tactile
		int [][] matrix = new int[Ernest.RESOLUTION_RETINA][8 + 1 + 3];
		Pair<Integer, Color>[] eyeFixation = null;
		eyeFixation = getRetina(mOrientation.z);
		
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
		{
			matrix[i][0] = eyeFixation[i].mLeft;
			matrix[i][1] = eyeFixation[i].mRight.getRed();
			matrix[i][2] = eyeFixation[i].mRight.getGreen();
			matrix[i][3] = eyeFixation[i].mRight.getBlue();
			// The second row is the place where Ernest is standing
			matrix[i][4] = 0;
			matrix[i][5] = m_blocks[Math.round(m_x)][Math.round(m_y)].seeBlock().getRed();
			matrix[i][6] = m_blocks[Math.round(m_x)][Math.round(m_y)].seeBlock().getGreen();
			matrix[i][7] = m_blocks[Math.round(m_x)][Math.round(m_y)].seeBlock().getBlue();
		}
		
		// Taste 
		
		matrix[0][8] = taste();
		
		// Kinematic (simulates sensors of body action) 
		
		if (m_schema.equals(">"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_FORWARD : Ernest.STIMULATION_KINEMATIC_BUMP);
		else if (m_schema.equals("^"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_LEFT_EMPTY : Ernest.STIMULATION_KINEMATIC_LEFT_WALL);
		else if (m_schema.equals("v"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_RIGHT_EMPTY : Ernest.STIMULATION_KINEMATIC_RIGHT_WALL);
		
		// Tactile
		
		int [] somatoMap = somatoMap();
		//for (int i = 0; i < 3; i++)
			//for (int j = 0; j < 3; j++)
		for (int i = 0; i < 9; i++)
				matrix[i][9] = somatoMap[i];
		
		// Circadian (information on day or night)
		
		matrix[2][8] = (isNight() ? 1 : 0);		

		String intention = m_ernest.step(matrix);
		return intention;
	}
	
	/**
	 * Enact the primitive schema chosen by Ernest.
	 * @return binary feedback. 
	 */
	public boolean enactSchema(String schema)
	{
		m_schema = schema;
		
		// A new interaction cycle is starting
		m_counter++;
		System.out.println("Step #" + m_counter + "=======");
		
		boolean status = true;
		
		if (schema.equals(""))
		{
			setChanged();
			notifyObservers2();			
			sleep(200);
			status = true;
		}
		else if (schema.equals("v"))
			status = turnRight();
		else if (schema.equals("^"))
			status = turnLeft();
		else if (schema.equals(">"))
			status = forward();

		// Trace the environmental data
		if (m_tracer != null)
		{
			Object environment = m_tracer.newEvent("environment", "position", m_counter);
			m_tracer.addSubelement(environment, "x", m_x + "");
			m_tracer.addSubelement(environment, "y", m_y + "");
			m_tracer.addSubelement(environment,"orientation", m_orientation + "");
		}		
	    return status;
	}

	/**
	 * Taste the square where Ernest is. 
	 * Suck the square if it is food or water. 
	 * @return 0 if nothing, 1 if water, 2 if food. 
	 */
	private int taste()
	{
		// Taste before sucking
		//int taste = getDirty(m_x,m_y); 

		// Sucking water or food if any
		// TODO: only suck water when thirsty and food when hungry.
		//if (taste == DIRTY)
		if (isFood(m_x,m_y))
			suck();

		int stimulation = ((isFood(m_x,m_y)) ? Ernest.STIMULATION_GUSTATORY_FISH : Ernest.STIMULATION_GUSTATORY_NOTHING);
		return stimulation;
	}
	
	/**
	 * Turn left. 
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnLeft(){

		m_Ir = -45;
		m_v = 0;
		return impulse(ACTION_LEFT);
	}
	
	/**
	 * Turn right.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnRight(){
		
		m_Ir = 45;
		m_v = 0;
		return impulse(ACTION_RIGHT);
	}
	
	/**
	 * Move forward.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean forward(){
		
		m_If=1f; //1.5f
		m_theta = 0;
		return impulse(ACTION_FORWARD);
	}

	/**
	 * Perform an action in the environment 
	 * @param act the action to perform
	 * @return true if bump, false if not bump. 
	 */
	public boolean impulse(int act){
		
		boolean statusL=true;
		boolean statusR=true;
		float step;                  // length of a step
		float HBradius=(float) 0.4;  // radius of Ernest hitbox 
		 
		int cell_x=Math.round(m_x);
		int cell_y=Math.round(m_y);
		
		boolean status1=true;         // vertical motion
		boolean status2=true;         // horizontal motion
		boolean status3=true;         // begin on a dirty cell
		boolean status4=true;         // reach a dirty cell (=false when reach dirty cell)
		boolean status5=true;         // touch a corner
		
		float vlmin;
		float vrmin;
		
		vlmin=(float) 0.1;
		vrmin=(float) 10; // when teta<10, Ernest is not turning anymore
		
		status3=(isAlga(cell_x,cell_y) || isFood(cell_x,cell_y) );
		
		while  ( ((m_v>vlmin || m_If>0) && statusL) ||  (Math.abs(m_theta)>vrmin  || m_Ir!=0) ){
			
			// set linear impulsion
			if (m_If>0){
				m_v=m_If;
				m_If=0;
			}
			else 
				m_v-= 0.01*m_v;
			
			if (m_v<=0.1) m_v=0;
			
			// set angular impulsion
			if (m_Ir!=0){
				m_theta=m_Ir;
				m_Ir=0;
			}
			else 
				m_theta-= 0.1*m_theta;
			
			//if (Math.abs(m_theta)<=0.1) m_theta=0;
			
	// compute new position
			
			// for linear movements
			double d;
			if (statusL){
				step=m_v/90;
				
				double dx= step*Math.sin(m_orientationAngle);
				double dy=-step*Math.cos(m_orientationAngle);
				cell_x=Math.round(m_x);
				cell_y=Math.round(m_y);
				m_x+=dx;
				m_y+=dy;
				mPosition.x = m_x;
				mPosition.y = (float) m_h - m_y;
			}
			
			// for angular movements
			m_orientation+=m_theta/10;
			if (m_orientation < 0)   m_orientation +=360;
			if (m_orientation >=360) m_orientation -=360;
			m_orientationAngle =  m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
			mOrientation.z = (float) (Math.PI/2 - m_orientationAngle);
		
			if (tempo)
				m_env.repaint();
				if (m_theta != 0)
					sleep((int)(20));
				if (m_v != 0){
				sleep((int)(1));
			}

			
	// compute state
		// for linear movement
			// current cell
			if (isAlga(cell_x,cell_y) || isFood(cell_x,cell_y)){
				if (status3 && !isAlga(cell_x,cell_y) && !isFood(cell_x,cell_y) ) status3=false;
				if (!status3 && (isAlga(cell_x,cell_y) || isFood(cell_x,cell_y))) status4=false;
			}
			// top cell
			
			//Vector3f northPos = new Vector3f(mPosition);
			//northPos.add(DIRECTION_NORTH);
			//Vector3f testPos = new Vector3f();
			//testPos.scaleAdd(1, mPosition, DIRECTION_NORTH);
			
			//if ( !affordWalk(northPos) && (m_y-HBradius) < ((float)cell_y-1+0.5) ){
			// top cell
			if ( (isWall(cell_x,cell_y-1)) && (m_y-HBradius) -((float)cell_y-1+0.5)<0 )
			{
				if ((mOrientation.z > (float)Math.PI/4 && mOrientation.z < 3*(float)Math.PI/4) ||
					(mOrientation.z < - 5*(float)Math.PI/4 && mOrientation.z > -7*(float)Math.PI/4))
					// It counts as a bump only if the angle is closer to perpendicular plus or minus PI/4
					status1=false;
				m_y+= ((float)cell_y-1+0.5) - (m_y-HBradius);
			}
			// right cell
			if ( (isWall(cell_x+1,cell_y)) && ((float)cell_x+1-0.5) -(m_x+HBradius)<0 ){
				if ((mOrientation.z > - (float)Math.PI/4 && mOrientation.z < (float)Math.PI/4) ||
					(mOrientation.z > 7 * (float)Math.PI/4))	
					status2=false;
				m_x-= (m_x+HBradius) - ((float)cell_x+1-0.5);
			}
			// bottom cell
			if ( (isWall(cell_x,cell_y+1)) && ((float)cell_y+1-0.5) -(m_y+HBradius)<0 ){
				if ((mOrientation.z < - (float)Math.PI/4 && mOrientation.z > - 3 *(float)Math.PI/4) ||
					(mOrientation.z > 5*(float)Math.PI/4 && mOrientation.z < 7 *(float)Math.PI/4))
					status1=false;
				m_y-= (m_y+HBradius) - ((float)cell_y+1-0.5);
			}
			// left cell
			if ( (isWall(cell_x-1,cell_y)) && (m_x-HBradius) -((float)cell_x-1+0.5)<0 ){
				if ((mOrientation.z > 3*(float)Math.PI/4 && mOrientation.z < 5*(float)Math.PI/4) ||
						(mOrientation.z < - 3*(float)Math.PI/4))
					status2=false;
				m_x+= ((float)cell_x-1+0.5) - (m_x-HBradius);
			}
			// top right
			d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
			d=Math.sqrt(d);
			if (isWall(cell_x+1,cell_y-1) && d-0.4<0){
				while (d-0.4<0){
					m_x-=0.01;
					m_y+=0.01;
					d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// bottom right
			d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y+1-0.5))*(m_y-(cell_y+1-0.5));
			d=Math.sqrt(d);
			if (isWall(cell_x+1,cell_y+1) && d-0.4<0){
				while (d-0.4<0){
					m_x-=0.01;
					m_y-=0.01;
					d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// bottom left
			d= (m_x-(cell_x-1+0.5))*(m_x-(cell_x-1+0.5))+(m_y-(cell_y+1-0.5))*(m_y-(cell_y+1-0.5));
			d=Math.sqrt(d);
			if (isWall(cell_x-1,cell_y+1) && d-0.4<0){
				while (d-0.4<0){
					m_x+=0.01;
					m_y-=0.01;
					d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// top left
			d= (m_x-(cell_x-1+0.5))*(m_x-(cell_x-1+0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
			d=Math.sqrt(d);
			if (isWall(cell_x-1,cell_y-1) && d-0.4<0){
				while (d-0.4<0){
					m_x+=0.01;
					m_y+=0.01;
					d= (m_x-(cell_x+1-0.5))*(m_x-(cell_x+1-0.5))+(m_y-(cell_y-1+0.5))*(m_y-(cell_y-1+0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			
//			m_tactileFrame.repaint();
			
			statusL = (status1 || status2) && status4;
		}
		
		
	// compute state for angular movement
		int adjacent_x = Math.round(m_x);
		int adjacent_y = Math.round(m_y);
		
		// Adjacent square
		if (m_orientation < ORIENTATION_UP_RIGHT && m_orientation >=ORIENTATION_UP_LEFT)
			adjacent_y = Math.round(m_y) - 1;
		if (m_orientation >=ORIENTATION_UP && m_orientation<ORIENTATION_RIGHT){
			adjacent_x = Math.round(m_x) + 1;
			adjacent_y = Math.round(m_y) - 1;
		}
		if (m_orientation >= ORIENTATION_UP_RIGHT && m_orientation <ORIENTATION_DOWN_RIGHT)
			adjacent_x = Math.round(m_x) + 1;
		if (m_orientation >= ORIENTATION_RIGHT && m_orientation <ORIENTATION_DOWN){
			adjacent_x = Math.round(m_x) + 1;
			adjacent_y = Math.round(m_y) + 1;
		}
		if (m_orientation >= ORIENTATION_DOWN_RIGHT && m_orientation <ORIENTATION_DOWN_LEFT)
			adjacent_y = Math.round(m_y) + 1;
		if (m_orientation >= ORIENTATION_DOWN && m_orientation <ORIENTATION_LEFT){
			adjacent_x = Math.round(m_x) - 1;
			adjacent_y = Math.round(m_y) + 1;
		}
		if (m_orientation >= ORIENTATION_DOWN_LEFT && m_orientation <ORIENTATION_UP_LEFT)
			adjacent_x = Math.round(m_x) - 1;
		if (m_orientation >= ORIENTATION_LEFT && m_orientation <ORIENTATION_UP){
			adjacent_x = Math.round(m_x) - 1;
			adjacent_y = Math.round(m_y) - 1;
		}
		
		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h)){
			if (isWall(adjacent_x, adjacent_y)){
				setAnim(adjacent_x,adjacent_y, ANIM_RUB); 
				statusR = false;
			}
		}
		else
			statusR = false;

		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h))
			setAnim(adjacent_x, adjacent_y, ANIM_NO);	
		
		setChanged();
		notifyObservers2();			
		
		//if (tempo) sleep(10);
		
		setChanged();
		notifyObservers2();
		
		
		
		if (!status4){
			m_v=0;
		}
		else if (!status1 || !status2){
			m_v=0;
		}
		
		if (act==0) 
			return status1 && status2; //  && status5; bumping corners is not bumping.
		else        return statusR;
	}
	
	

	/**
	 * Paint Ernest as a shark.
	 * @param g The graphic object for painting.
	 */
	public void paintAgent(Graphics2D g2d,int x,int y,double sx,double sy)
	{
		
		// The orientation

		AffineTransform orientation = new AffineTransform();
		orientation.translate(x,y);
		orientation.rotate(m_orientationAngle);
		orientation.scale(sx,sy);
		g2d.transform(orientation);
		//AffineTransform bodyReference = g2d.getTransform();

		// Eye Colors
		
		Pair<Integer, Color>[] eyeFixation = null;
		Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA];
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
			pixelColor[i] = UNANIMATED_COLOR;
		
		// body Color
		
		Color[][] somatoMapColor = new Color[3][3];
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				somatoMapColor[i][j] = UNANIMATED_COLOR;
		
		// Animate Ernest when he is alive

		if (m_ernest != null)
		{
			// Eye color
			eyeFixation = getRetina(mOrientation.z);
			//eyeFixation=rendu(false);
			for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
			{
				pixelColor[i] = eyeFixation[i].mRight;
			}
			
			// Somatomap color
			int [] somatoMap = somatoMap();
			somatoMapColor[2][2] = new Color(somatoMap[0]);
			somatoMapColor[2][1] = new Color(somatoMap[1]);
			somatoMapColor[2][0] = new Color(somatoMap[2]);
			somatoMapColor[1][0] = new Color(somatoMap[3]);
			somatoMapColor[0][0] = new Color(somatoMap[4]);
			somatoMapColor[0][1] = new Color(somatoMap[5]);
			somatoMapColor[0][2] = new Color(somatoMap[6]);
			somatoMapColor[1][2] = new Color(somatoMap[7]);
			somatoMapColor[1][1] = new Color(somatoMap[8]);

//			for (int i = 0; i < 3; i++)
//				for (int j = 0; j < 3; j++)
//				{
//					somatoMapColor[i][j] = new Color(somatoMap[i][j]);
//				}
		}
		
		// The shark body

		Area sharkMask = sharkmask();
		
		// Retina pixel
		
		Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
		
		// The somatomap
		
		Rectangle2D.Double[][] somatoMap = new Rectangle2D.Double[3][3];
		somatoMap[0][0] = new Rectangle2D.Double(-49, -47, 42, 40);
		somatoMap[1][0] = new Rectangle2D.Double( -8, -47, 16, 40);
		somatoMap[2][0] = new Rectangle2D.Double(  7, -47, 42, 40);
		
		somatoMap[0][1] = new Rectangle2D.Double(-49,  -8, 42, 16);
		somatoMap[1][1] = new Rectangle2D.Double( -8,  -8, 16, 16);
		somatoMap[2][1] = new Rectangle2D.Double(  7,  -8, 42, 16);

		somatoMap[0][2] = new Rectangle2D.Double(-49,   6, 42, 42);
		somatoMap[1][2] = new Rectangle2D.Double( -8,   6, 16, 42);
		somatoMap[2][2] = new Rectangle2D.Double(  7,   6, 42, 42);

		// Draw the somatomap
		
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
			{
				g2d.setColor(somatoMapColor[i][j]);
				g2d.fill(somatoMap[i][j]);
			}
		
		// Draw the body
		
		g2d.setStroke(new BasicStroke(2f));		
		g2d.setColor(m_blocks[Math.round(m_x)][Math.round(m_y)].seeBlock() );
		g2d.setColor(new Color(255,255,255)) ;
		g2d.fill(sharkMask);

		
		// Draw the retina
		
		AffineTransform transformColliculus = new AffineTransform();
		transformColliculus.rotate(0);
		transformColliculus.translate(0,-22);
		g2d.transform(transformColliculus);
		AffineTransform RetinaReference = g2d.getTransform();
		AffineTransform transformSegment = new AffineTransform();
		g2d.transform(transformSegment);
		transformSegment.rotate( - Math.PI / Ernest.RESOLUTION_RETINA);
		g2d.setColor(Color.BLACK);
		
		g2d.setTransform(RetinaReference);
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
		{
			g2d.setColor(pixelColor[i]);
			g2d.fill(pixelIn);
			g2d.transform(transformSegment);
		}
	}

	/**
	 * Paint Ernest's observation.
	 * @param g The graphic object for painting.
	 */
	public void paintDream(Graphics2D g2d,int x,int y,double sx,double sy)
	{

		AffineTransform scale       = new AffineTransform();
		AffineTransform translation = new AffineTransform();
		scale.scale(sx,sy);
		translation.translate(x,y);

		g2d.transform(translation);
		g2d.transform(scale);
		
		Color eyeColor = UNANIMATED_COLOR;
		Color kinematicColor = Color.WHITE;
		float direction = 5.5f;
		float span = (float)Math.PI / 12;
		
		// body Color
		
		Color[][] somatoMapColor = new Color[3][3];
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				somatoMapColor[i][j] = Color.WHITE;
		
		// Animate Ernest when he is alive

		if (m_ernest != null)
		{
			// Eye
//			if (m_ernest.getObservation().getSalience() != null)
//			{
//				span = m_ernest.getObservation().getSalience().getSpan();
//				direction = m_ernest.getObservation().getSalience().getDirection() / 10f - span / 2f + .5f;
//				eyeColor = new Color(m_ernest.getObservation().getSalience().getBundle().getVisualStimulation().getValue());
//				//eyeColor = new Color(m_ernest.getObservation().getSalience().getColor().getRGB());
//			}
						
			// Somatomap color
			for (int i = 0; i < 3; i++)
				for (int j = 0; j < 3; j++)
					somatoMapColor[i][j] = new Color(m_ernest.getValue(i, j));
			
//			if (Ernest.STIMULATION_KINEMATIC_BUMP.equals(m_ernest.getObservation().getKinematic()))
//				kinematicColor = Color.RED;
		}
		
		// Retina pixel
		
		Arc2D.Double eye = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA * span , Arc2D.PIE);
		
		// The somatomap
		
		Rectangle2D.Double[][] somatoMap = new Rectangle2D.Double[3][3];
		somatoMap[0][0] = new Rectangle2D.Double(-49, -47, 42, 40);
		somatoMap[1][0] = new Rectangle2D.Double( -8, -47, 16, 40);
		somatoMap[2][0] = new Rectangle2D.Double(  7, -47, 42, 40);
		
		somatoMap[0][1] = new Rectangle2D.Double(-49,  -8, 42, 16);
		somatoMap[1][1] = new Rectangle2D.Double( -8,  -8, 16, 16);
		somatoMap[2][1] = new Rectangle2D.Double(  7,  -8, 42, 16);

		somatoMap[0][2] = new Rectangle2D.Double(-49,   6, 42, 42);
		somatoMap[1][2] = new Rectangle2D.Double( -8,   6, 16, 42);
		somatoMap[2][2] = new Rectangle2D.Double(  7,   6, 42, 42);
		
		Ellipse2D.Double kinematic = new Ellipse2D.Double( -8, -40, 15, 15);

		// Draw the somatomap
		
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
			{
				g2d.setColor(somatoMapColor[i][j]);
				g2d.fill(somatoMap[i][j]);
			}
		
		if (kinematicColor.equals(Color.RED))
		{
			g2d.setColor(kinematicColor);
			g2d.fill(kinematic);
		}
		
		// Draw the shark
		
		Area sharkMask = sharkmask();

		g2d.setColor(WALL_COLOR);
		//g2d.setColor(new Color(Ernest.COLOR_WALL.getRGB()));
		if (isNight()) g2d.setColor(new Color(80,80,80)) ;
		g2d.fill(sharkMask);
		
		//g2d.setColor(eyeColor);
		//g2d.fill(new Rectangle2D.Double( 25,  -35, 16, 16));

		
	}


	private Area sharkmask()
	{
		// The shark body

		GeneralPath body = new GeneralPath();
		body.append(new CubicCurve2D.Double(0,-40, -30,-40, -5, 45, 0, 45), false);
		body.append(new CubicCurve2D.Double(0, 45,   5, 45, 30,-40, 0,-40), false);
		
		GeneralPath leftPectoralFin = new GeneralPath();
		leftPectoralFin.append(new CubicCurve2D.Double(-15, -15, -30, -10, -40, 0, -40, 20), false);
		leftPectoralFin.append(new CubicCurve2D.Double(-40,  20, -30,  10, -20, 8, -10, 10), true);

		GeneralPath leftPelvicFin = new GeneralPath();
		leftPelvicFin.append(new CubicCurve2D.Double(-10, 15, -15, 18, -20, 25, -15, 30), false);
		leftPelvicFin.append(new CubicCurve2D.Double(-15, 30,  -10, 25, -10,  25,  -5, 28), true);

		GeneralPath rightPectoralFin = new GeneralPath();
		rightPectoralFin.append(new CubicCurve2D.Double(15, -15, 30, -10, 40, 0, 40, 20), false);
		rightPectoralFin.append(new CubicCurve2D.Double(40,  20, 30,  10, 20, 8, 10, 10), true);

		GeneralPath rightPelvicFin = new GeneralPath();
		rightPelvicFin.append(new CubicCurve2D.Double(10, 15, 15, 18, 20, 25, 15, 30), false);
		rightPelvicFin.append(new CubicCurve2D.Double(15, 30, 10, 25, 10, 25,  5, 28), true);

		GeneralPath caudalFin = new GeneralPath();
		caudalFin.append(new CubicCurve2D.Double(10, 50, 15, 20, -15, 20, -10, 50), false);
		caudalFin.append(new CubicCurve2D.Double(-10, 50, -15, 30, 15, 30, 10, 50), false);

		Area shark = new Area(body);
		shark.add(new Area(leftPectoralFin));shark.add(new Area(leftPelvicFin));
		shark.add(new Area(rightPectoralFin));shark.add(new Area(rightPelvicFin));
		
		Area sharkMask = new Area(new Rectangle2D.Double(-80, -80, 160, 160));
		sharkMask.subtract(shark);

		return sharkMask;
	}
	
}
