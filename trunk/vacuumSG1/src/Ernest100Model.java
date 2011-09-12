

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
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

import ernest.*;
import tracing.*;


/**************************************
 * A Model for Ernest 10
 * Ernest can turn PI/4 and move in diagonal.
 * See http://e-ernest.blogspot.com/2011/02/ernestor.html
 * and http://e-ernest.blogspot.com/2011/02/ernest-84.html
 * @author ogeorgeon
 **************************************/
public class Ernest100Model extends ErnestModel 
{
	protected static final int ORIENTATION_UP_RIGHT    = 45; 
	protected static final int ORIENTATION_DOWN_RIGHT  = 135; 
	protected static final int ORIENTATION_DOWN_LEFT   = 225; 
	protected static final int ORIENTATION_UP_LEFT     = 315; 
	public static Color UNANIMATED_COLOR = Color.GRAY;
	
	private double m_rotation_speed = Math.PI/Ernest.RESOLUTION_RETINA;
	
	private long time1=System.currentTimeMillis();
	private long time2=System.currentTimeMillis();
	
	public float m_v;                       // linear speed
	public float m_theta;					// angular speed
	public float m_If,m_Ir;   				// impulsion counters
	public int lastAction;
	
	public EnvironnementFrame m_env;
	public InternalStatesFrame m_int;
	public InternalMap m_map;
	public ObjectMemory m_objMemory;
	//public PatternMap m_patternMap;
	//public PatternMappingFrame m_patternFrame;
	
	public ArrayList<Action> m_actionList;
	public float distance=0;
	public float angle=0;
	//public int objectType=0;
	
	private EyeView eye;
	
	public Color frontColor;
	
	public float[] m_tactilePressure;
	public float[] m_tactileObject;
	
	public boolean tempo=false;
	public boolean continuum=true;
	
	/**
	 * Initialize the agent in the grid
	 */
	public void init(String f) throws Exception
	{
		// Initialize the model
		super.init(f);
		setEyeAngle(Math.PI/4);
		setOrientationStep(45);

		setChanged();
		notifyObservers2();
		
		m_actionList=new ArrayList<Action>();

		m_tactilePressure=new float[32];
		m_tactileObject=new float[32];
		
		m_objMemory=new ObjectMemory();
		m_map=new InternalMap(m_objMemory);
		
		if (!load(m_actionList)){
			m_actionList.clear();
			m_actionList.add(new Action("forward",10,120,150,m_objMemory));
			m_actionList.add(new Action("turnLeft",1 ,180,180,m_objMemory));
			m_actionList.add(new Action("turnRight",1 ,180,180,m_objMemory));
		}
		m_int=new InternalStatesFrame(m_actionList);
		
		//m_patternMap=new PatternMap();
		//m_patternFrame=new PatternMappingFrame(m_patternMap);
		
		eye=new EyeView(m_map);
		
		frontColor=new Color(0,0,0);
	}

	/**
	 * @return The version of Ernest
	 */
	public String getVersion()
	{
		return "Ernest 10.3";
	}
	
	/**
	 * Initialize the Ernest agent.
	 */
	public void initErnest()
	{
		m_ernest.add(new Ernest());
		
		System.out.println(m_ernest.size());
		
		m_sensorymotorSystem = new Visual100SensorymotorSystem();
		m_tracer = new XMLTracer("trace.xml");
		//m_tracer = new XMLStreamTracer("http://vm.liris.cnrs.fr:34080/abstract/Ernest-trace-player-tr/php/");
		//m_tracer = new XMLStreamTracer("http://134.214.142.59/abstract/Ernest-trace-player-test-tr/php/");
		
		// Initialize the Ernest === 
		
		m_ernest.get(0).setParameters(5, 1, 4);
		m_ernest.get(0).setTracer(m_tracer);
		m_ernest.get(0).setSensorymotorSystem(m_sensorymotorSystem);
		


		// Ernest's inborn primitive interactions
		m_sensorymotorSystem.addPrimitiveAct(">", true,   100); // Move
		m_sensorymotorSystem.addPrimitiveAct(">", false, -100); // Bump 
		
		if (continuum){
			m_sensorymotorSystem.addPrimitiveAct("^", true,   -50); // Left toward empty
			m_sensorymotorSystem.addPrimitiveAct("^", false,  -70); // Left toward wall

			m_sensorymotorSystem.addPrimitiveAct("v", true,   -50); // Right toward empty
			m_sensorymotorSystem.addPrimitiveAct("v", false,  -70); // Right toward wall
		}
		else{
			m_sensorymotorSystem.addPrimitiveAct("^", true,   -10); // Left toward empty
			m_sensorymotorSystem.addPrimitiveAct("^", false,  -20); // Left toward wall

			m_sensorymotorSystem.addPrimitiveAct("v", true,   -10); // Right toward empty
			m_sensorymotorSystem.addPrimitiveAct("v", false,  -20); // Right toward wall
		}
		System.out.println("Ernest initialized") ;
	}

	
	public void setEnvironnement(EnvironnementFrame env){
		m_env=env;
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
		//m_tracer.startNewEvent(m_counter);

		// See the environment
		int [][] matrix = new int[Ernest.RESOLUTION_RETINA][8 + 1 + 3];
		EyeFixation[] eyeFixation = null;
		//eyeFixation = retina(Math.PI/2 - m_orientationAngle);
		eyeFixation = rendu(false);
		
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
		{
			matrix[i][0] = eyeFixation[i].getDistance();
			matrix[i][1] = eyeFixation[i].getColor().getRed();
			matrix[i][2] = eyeFixation[i].getColor().getGreen();
			matrix[i][3] = eyeFixation[i].getColor().getBlue();
			// The second row is the place where Ernest is standing
			matrix[i][4] = 0;
			matrix[i][5] = getBackgroundColor(cell(m_x),cell(m_y)).getRed();
			matrix[i][6] = getBackgroundColor(cell(m_x),cell(m_y)).getGreen();
			matrix[i][7] = getBackgroundColor(cell(m_x),cell(m_y)).getBlue();
		}
		
		// Taste 
		
		matrix[0][8] = taste();
		
		// Kinematic (simulates sensors of body action) 
		
		if (m_schema.equals(">"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_FORWARD.getValue() : Ernest.STIMULATION_KINEMATIC_BUMP.getValue());
		else if (m_schema.equals("^"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_LEFT_EMPTY.getValue() : Ernest.STIMULATION_KINEMATIC_LEFT_WALL.getValue());
		else if (m_schema.equals("v"))
			matrix[1][8] = (status ? Ernest.STIMULATION_KINEMATIC_RIGHT_EMPTY.getValue() : Ernest.STIMULATION_KINEMATIC_RIGHT_WALL.getValue());
		
		// Tactile
		
		int [][] somatoMap = somatoMap();
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				matrix[i][9 + j] = somatoMap[i][j];
		
		// Circadian (information on day or night)
		
		matrix[2][8] = (isNight() ? 1 : 0);		

		String intention = m_ernest.get(0).step(matrix);
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
		
		if (isNight())
		{
			setChanged();
			notifyObservers2();			
			sleep(100);
		}
		else
		{
			if (schema.equals(""))
			{
				setChanged();
				notifyObservers2();			
				//if (tempo) sleep(10);
				status = true;
			}
			else if (schema.equals("v"))
				status = turnRight();
			else if (schema.equals("^"))
				status = turnLeft();
			else if (schema.equals(">"))
				status = forward();
	
			// Trace the environmental data
			//Object environment = m_tracer.newEvent("environment", "position", m_counter);
			//m_tracer.addSubelement(environment, "x", m_x + "");
			//m_tracer.addSubelement(environment, "y", m_y + "");
			//m_tracer.addSubelement(environment,"orientation", m_orientation + "");
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
		int taste = getDirty(cell(m_x),cell(m_y)); 

		// Sucking water or food if any
		// TODO: only suck water when thirsty and food when hungry.
		if (taste == DIRTY) 	
			suck();

		return taste;
	}
	
	/**
	 * Turn left. 
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnLeft(){
		m_eyeOrientation = 0;
		if (continuum){
			rendu(true);
			angle=m_map.imax;
			float orientation=(m_actionList.get(1)).selectOutput(angle,frontColor)+1;
			m_Ir=-orientation;
		}
		else{
			float rand= (float) (Math.random()*20-10);
			m_Ir=-45+rand;
		}
		
		lastAction=1;
		return impulse(1);
	}
	
	/**
	 * Turn right.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnRight(){
		m_eyeOrientation = 0;
		
		if (continuum){
			rendu(true);
			angle=m_map.imax;
			float orientation=(m_actionList.get(2)).selectOutput(angle,frontColor)+1;
			m_Ir=+orientation;
		}
		else{
			float rand= (float) (Math.random()*20-10);
			m_Ir=45+rand;
		}
		
		lastAction=2;
		return impulse(2);
	}
	
	/**
	 * return the nearest integer of a float
	 */
	private int cell(float a){
		if (a-(int)a <=0.5) return (int)a;
		else                return (int)a+1;
	}
	
	/**
	 * Move forward.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean forward(){
		
		if (continuum){
			rendu(true);
			m_If=(float) ((m_actionList.get(0)).selectOutput(distance,frontColor)+0.1);
		}
		else{
			float rand= (float) (Math.random()-0.5);
			m_If=1+rand;
		}
		
		lastAction=0;
		return impulse(0);
	}

	
	////////////////////////////////////////////////////
	//
	////////////////////////////////////////////////////
	public boolean impulse(int act){
		
		boolean statusL=true;
		boolean statusR=true;
		float step;                  // length of a step
		float HBradius=(float) 0.4;  // radius of Ernest hitbox 
		 
		float maxPoint=m_map.max;
		
		int cell_x=cell(m_x);
		int cell_y=cell(m_y);
		
		boolean status1=true;         // vertical motion
		boolean status2=true;         // horizontal motion
		boolean status3=true;         // begin on a dirty cell
		boolean status4=true;         // reach a dirty cell (=false when reach dirty cell)
		boolean status5=true;         // touch a corner
		
		float dist=0;
		float a=0;
		float previousDistance=distance;
		Color previousColor=frontColor;
		
		int i=10;
		int j=5;
		
		float vlmin;
		float vrmin;
		
		if (continuum) {
			vlmin=(float) 0.1;
			vrmin=(float) 0.1;
		}
		else{
			vlmin=0;
			vrmin=0;
		}
		
		
		status3=isDirty(cell_x,cell_y);
		
		while  ( ((m_v>vlmin || m_If>0) && statusL) ||  (Math.abs(m_theta)>vrmin  || m_Ir!=0) ){
			
			// set linear impulsion
			if (m_If>0){
				m_v=m_If;
				m_If=0;
			}
			else 
				if (continuum) m_v-= 0.01*m_v;
				else if (i>0) i--;
				     else m_v=0;
			
			if (m_v<=0.1) m_v=0;
			
			// set angular impulsion
			if (m_Ir!=0){
				m_theta=m_Ir;
				m_Ir=0;
			}
			else 
				if (continuum) m_theta-= 0.1*m_theta;
				else if (j>0) j--;
				     else m_theta-=m_theta/10;
			
			//if (Math.abs(m_theta)<=0.1) m_theta=0;
			
	// compute new position
			
			// for linear movements
			double d;
			if (statusL){
				if (continuum) step=m_v/90;
				else           step=m_v/10;
				
				double dx= step*Math.sin(m_orientationAngle);
				double dy=-step*Math.cos(m_orientationAngle);
				cell_x=cell(m_x);
				cell_y=cell(m_y);
				dist+=step;
				m_x+=dx;
				m_y+=dy;
			}
			
			// for angular movements
			if (continuum){
				m_orientation+=m_theta/10;
				a+=m_theta/10;
			}
			else m_orientation+=m_theta/9;
			if (m_orientation < 0)   m_orientation +=360;
			if (m_orientation >=360) m_orientation -=360;
			m_orientationAngle =  m_orientation * Math.PI/2 / ORIENTATION_RIGHT;
			
			
	// compute state
		// for linear movement
			// current cell
			if (isDirty(cell_x,cell_y)){
				if (status3 && !isDirty(cell_x,cell_y)) status3=false;
				if (!status3 && isDirty(cell_x,cell_y)) status4=false;
			}
			// top cell
			if ( (isWall(cell_x,cell_y-1)) && (m_y-HBradius) -((float)cell_y-1+0.5)<0 ){
				status1=false;
				m_y+= ((float)cell_y-1+0.5) - (m_y-HBradius);
			}
			// right cell
			if ( (isWall(cell_x+1,cell_y)) && ((float)cell_x+1-0.5) -(m_x+HBradius)<0 ){
				status2=false;
				m_x-= (m_x+HBradius) - ((float)cell_x+1-0.5);
			}
			// bottom cell
			if ( (isWall(cell_x,cell_y+1)) && ((float)cell_y+1-0.5) -(m_y+HBradius)<0 ){
				status1=false;
				m_y-= (m_y+HBradius) - ((float)cell_y+1-0.5);
			}
			// left cell
			if ( (isWall(cell_x-1,cell_y)) && (m_x-HBradius) -((float)cell_x-1+0.5)<0 ){
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
			if (tempo){
				rendu(false);
				m_env.repaint();
				m_int.repaint();
				sleep((int)(1));
			}
			statusL=status1 && status2 && status4;
		}
		
		
	// compute state for angular movement
		int adjacent_x = cell(m_x);
		int adjacent_y = cell(m_y);
		
		// Adjacent square
		if (m_orientation < ORIENTATION_UP_RIGHT && m_orientation >=ORIENTATION_UP_LEFT)
			adjacent_y = cell(m_y) - 1;
		if (m_orientation >=ORIENTATION_UP && m_orientation<ORIENTATION_RIGHT){
			adjacent_x = cell(m_x) + 1;
			adjacent_y = cell(m_y) - 1;
		}
		if (m_orientation >= ORIENTATION_UP_RIGHT && m_orientation <ORIENTATION_DOWN_RIGHT)
			adjacent_x = cell(m_x) + 1;
		if (m_orientation >= ORIENTATION_RIGHT && m_orientation <ORIENTATION_DOWN){
			adjacent_x = cell(m_x) + 1;
			adjacent_y = cell(m_y) + 1;
		}
		if (m_orientation >= ORIENTATION_DOWN_RIGHT && m_orientation <ORIENTATION_DOWN_LEFT)
			adjacent_y = cell(m_y) + 1;
		if (m_orientation >= ORIENTATION_DOWN && m_orientation <ORIENTATION_LEFT){
			adjacent_x = cell(m_x) - 1;
			adjacent_y = cell(m_y) + 1;
		}
		if (m_orientation >= ORIENTATION_DOWN_LEFT && m_orientation <ORIENTATION_UP_LEFT)
			adjacent_x = cell(m_x) - 1;
		if (m_orientation >= ORIENTATION_LEFT && m_orientation <ORIENTATION_UP){
			adjacent_x = cell(m_x) - 1;
			adjacent_y = cell(m_y) - 1;
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
		
		
		rendu(true);
		
	// define reward for linear movement
		int reward=0;
		
		if (act==0){
		// define the "reward" for wall objects
			if (previousColor.equals(new Color(0,128,  0)) ||
					previousColor.equals(new Color(0,230, 92)) ||
					previousColor.equals(new Color(0,230,161)) ){
				if (!status1 || !status2 || !status5) reward=-100;
				else                      reward= 100;
			}
			else{
				if (!status4){
					reward=(int) (100- (Math.max(0, m_v*m_v-0.1)*2) );
				}
				else{
					reward= (int) (100 - (distance*distance*2));
				}		
			}
			if (continuum) m_actionList.get(0).setResults(reward);
		}
	// define reward for angular movement
		else{
			//reward= 100- Math.abs(m_map.imax-90)*2;
			reward=(int) (100 - Math.abs( -(angle-90) + a )*4);
			// point lost
			if (m_map.max+1<maxPoint) reward=-100;
			// new point
			if (m_map.max>maxPoint+1) reward= 100;
			
			if (continuum){
				if (act==1){
					m_actionList.get(1).setResults(reward);
				}
				if (act==2){
					m_actionList.get(2).setResults(reward);
				}
			}
		}
		
		//sleep((int)(70));
		//m_int.saveImage();
		
		//sleep((int)(10));
		//m_env.saveImage();
		
		if (!status4){
			if (previousColor.equals(new Color(150, 128, 255))) m_objMemory.setValue(previousColor, 100);
			else                                                m_objMemory.setValue(previousColor,  20);
			
			m_v=0;
		}
		else if (!status1 || !status2){
			m_objMemory.setValue(frontColor,-100);
			m_v=0;
		}
		
		rendu(true);
		
		if (act==0) return status1 && status2 && status5;
		else        return statusR;
	}
	
	
	
	////////////////////////////////////////////
	//
	////////////////////////////////////////////
	public void touchEnvironment(){
		
	}
	
	//******************************************
	////////////////////////////////////////////
	//******************************************
	protected EyeFixation[] rendu(boolean setdistance){
		double[] r    = new double[360];
		double[] r2   = new double[180];
		double[] zMap = new double[360];
		Color[] colorMap =new Color[360];
		Color[] colorMap2=new Color[180];
		int[] corner = new int[360];
		int[] corner2= new int[180];
		
		EyeFixation[] retina= new EyeFixation[Ernest.RESOLUTION_RETINA];
		
		double d=0;
		double d1,d2,d3;
		double a1,a2,a3;
		
		double imin,iplus,jmin,jplus;
		double imin2,jmin2;
		
		int Im_x=cell(m_x);
		int Im_y=cell(m_y);
		
		for (int i=0;i<360;i++){
			zMap[i]=1000;
			r[i]=200;
			colorMap[i]=new Color(0,0,0);
		}
		
		int sight=20;
		
		int orientationDeg= (int)(m_orientationAngle * 180 / Math.PI);
		
		
		for (int i=0;i<sight;i++){
			for (int j=0;j<sight;j++){
				
				// cells on the top right side
				if ( (i>0)&& (Im_x+i<m_w) && (Im_y-j>=0) ){
					if (isWall(Im_x+i,Im_y-j) || isDirty(Im_x+i,Im_y-j) ){
						Color bgc = getBackgroundColor(Im_x+i,Im_y-j);
						
						imin =(double)i-0.5 - (m_x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 - (m_x-Im_x);
						jmin =(double)j-0.5 + (m_y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 + (m_y-Im_y);
						
						d1=  imin2 + jmin2;
						d1=Math.sqrt(d1);
						d2=  imin2 + (jplus*jplus);
						d2=Math.sqrt(d2);
						d3=  (iplus*iplus) + jmin2;
						d3=Math.sqrt(d3);
						
						a1=  Math.toDegrees( Math.acos( jmin/d1));
						a2=  Math.toDegrees( Math.acos( jplus/d2));
						a3=  Math.toDegrees( Math.acos( jmin/d3));
						
						
				    	int ai1=(int)a1;
				    	int ai2=(int)a2;
				    	int ai3=(int)a3;
						
						for (int k=ai2;k<=ai1;k++){
							d= d2*10 +   (d1-d2)*10*(k-ai2)/(ai1-ai2);
							if (zMap[k]>d){
								
								r[k]=d;
				    			zMap[k]= d;
				    			colorMap[k]=bgc;
				    			
				    			if      (k==ai2) corner[k]=1;
				    			else if (k==ai1) corner[k]=2;
				    			else             corner[k]=0;
				    			
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d= d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
							if (zMap[k]>d){
								
								r[k]=d;
								zMap[k]= d;
				    			colorMap[k]=bgc;

				    			if      (k==ai1) corner[k]=1;
				    			else if (k==ai3) corner[k]=2;
				    			else             corner[k]=0;
							}
						}
						
					}
				}
				
				
				// cells on the bottom right side
				if ( (j>0) && (Im_x+i<m_w) && (Im_y+j<m_h) ){
					if (isWall(Im_x+i,Im_y+j) || isDirty(Im_x+i,Im_y+j) ){
						Color bgc = getBackgroundColor(Im_x+i,Im_y+j);
						
						imin =(double)i-0.5 - (m_x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 - (m_x-Im_x);
						jmin =(double)j-0.5 - (m_y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 - (m_y-Im_y);
						
						d1=  imin2 + jmin2;
						d1=Math.sqrt(d1);
						d2=  (iplus*iplus) + jmin2;
						d2=Math.sqrt(d2);
						d3=  imin2 + (jplus*jplus);
						d3=Math.sqrt(d3);
						
						a1=  Math.toDegrees( Math.acos( jmin/d1));
						a2=  Math.toDegrees( Math.acos( jmin/d2));
						a3=  Math.toDegrees( Math.acos( jplus/d3));
						
				    	int ai1,ai2,ai3;
				    	
				    	if (i-0.5>=0){
				    		ai1=180-(int)a1;
				    		ai3=180-(int)a3;
				    	}
				    	else{
				    		ai1=180+(int)a1;
				    		ai3=180+(int)a3;
				    	}
				    	ai2=180-(int)a2;
						
						for (int k=ai2;k<=ai1;k++){
							d= ( d2*10 +   (d1-d2)*10*(k-ai2)/(ai1-ai2));
							if (zMap[k]>d){
								r[k]=d;
								zMap[k]= d;
								colorMap[k]=bgc;
								
								if      (k==ai2) corner[k]=1;
				    			else if (k==ai1) corner[k]=2;
				    			else             corner[k]=0;
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d= ( d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1));
							if (zMap[k]>d){
								r[k]=d;
								zMap[k]= d;
								colorMap[k]=bgc;
								if      (k==ai1) corner[k]=1;
				    			else if (k==ai3) corner[k]=2;
				    			else             corner[k]=0;
							}
						}
						
					}
				}
				
				
				// cells on the bottom left side
				if ( (i>0) && (Im_x-i>=0) && (Im_y+j<m_h) ){
					if (isWall(Im_x-i,Im_y+j) || isDirty(Im_x-i,Im_y+j) ){
						Color bgc = getBackgroundColor(Im_x-i,Im_y+j);
						
						imin =(double)i-0.5 + (m_x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (m_x-Im_x);
						jmin =(double)j-0.5 - (m_y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 - (m_y-Im_y);
						
						d1=  imin2 + jmin2;
						d1=Math.sqrt(d1);
						d2=  imin2 + (jplus*jplus);
						d2=Math.sqrt(d2);
						d3=  (iplus*iplus) + jmin2;
						d3=Math.sqrt(d3);
						
						a1=  Math.toDegrees( Math.acos( jmin/d1));
						a2=  Math.toDegrees( Math.acos( jplus/d2));
						a3=  Math.toDegrees( Math.acos( jmin/d3));
						
				    	int ai1=180+(int)a1;
				    	int ai2=180+(int)a2;
				    	int ai3=180+(int)a3;
						
						for (int k=ai2;k<=ai1;k++){
							d=   d2*10 +   (d1-d2)*10*(k-ai2)/(ai1-ai2);
							if (zMap[k]>d){
								r[k]=d;
								zMap[k]=d;
								colorMap[k]=bgc;
								if      (k==ai2) corner[k]=1;
				    			else if (k==ai1) corner[k]=2;
				    			else             corner[k]=0;
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d=  d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
							if (zMap[k]>d){
								r[k]=d;
								zMap[k]=d;
								colorMap[k]=bgc;
								if      (k==ai1) corner[k]=1;
				    			else if (k==ai3) corner[k]=2;
				    			else             corner[k]=0;
							}
						}
						
					}
				}
				
				// cells exactly on the top
				if ( (j>0) && (i==0) && (Im_y-j>=0) ){
					if (isWall(Im_x-i,Im_y-j) || isDirty(Im_x-i,Im_y-j) ){
						Color bgc = getBackgroundColor(Im_x-i,Im_y-j);
						
						imin =(double)i-0.5 + (m_x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (m_x-Im_x);
						jmin =(double)j-0.5 + (m_y-Im_y);
						jmin2=jmin*jmin;
						
						d1=  imin2 + jmin2;
						d1=Math.sqrt(d1);
						d2=  (iplus*iplus) + jmin2;
						d2=Math.sqrt(d2);
						
						a1=  Math.toDegrees( Math.acos( jmin/d1));
						a2=  Math.toDegrees( Math.acos( jmin/d2));

						
						int ai1,ai2;
						ai1=(int)a1;
				    	ai2=360-(int)a2;
				    	if (ai2==360) ai2=359;
				    	
				    	int count=0;
				    	for (int k=ai2;k<360;k++){
				    		d= d2*10 +   (d1-d2)*10*(k-ai2)/((ai1-ai2+360)%360);
				    		if (zMap[k]>d){
				    			r[k]=d;
				    			zMap[k]= d;
				    			colorMap[k]=bgc;
				    			if      (k==ai2) corner[k]=1;
				    			else             corner[k]=0;
							}
				    		count++;
				    	}
				    	for (int k=0;k<=ai1;k++){
				    		d= d2*10 +   (d1-d2)*10*(k+count)/((ai1-ai2+360)%360);
				    		if (zMap[k]>d){
				    			r[k]=d;
				    			zMap[k]= d;
				    			colorMap[k]=bgc;
				    			if (k==ai1) corner[k]=2;
				    			else        corner[k]=0;
				    			
							}
				    	}
				    	
					}
				}
				
				// cells on the top left side
				if ( (j>0) && (i>0) && (Im_x-i>=0) && (Im_y-j>=0) ){
					if (isWall(Im_x-i,Im_y-j) || isDirty(Im_x-i,Im_y-j) ){
						Color bgc = getBackgroundColor(Im_x-i,Im_y-j);
						
						imin =(double)i-0.5 + (m_x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (m_x-Im_x);
						jmin =(double)j-0.5 + (m_y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 + (m_y-Im_y);
						
						d1=  imin2 + jmin2;
						d1=Math.sqrt(d1);
						d2=  (iplus*iplus) + jmin2;
						d2=Math.sqrt(d2);
						d3=  imin2 + (jplus*jplus);
						d3=Math.sqrt(d3);
						
						a1=  Math.toDegrees( Math.acos( jmin/d1));
						a2=  Math.toDegrees( Math.acos( jmin/d2));
						a3=  Math.toDegrees( Math.acos( jplus/d3));
						
						int ai1,ai2,ai3;
						ai1=360-(int)a1;
						ai3=360-(int)a3;
						if (ai1==360) ai1=359;
						if (ai3==360) ai3=359;
				    	ai2=360-(int)a2;
						
				    	for (int k=ai2;k<=ai1;k++){
				    		d= d2*10 +   (d1-d2)*10*(k-ai2)/(ai1-ai2);
				    		if (zMap[k]>d){
				    			r[k]=d;
				    			zMap[k]= d;
				    			colorMap[k]=bgc;
				    			if      (k==ai2) corner[k]=1;
				    			else if (k==ai1) corner[k]=2;
				    			else             corner[k]=0;
							}
				    	}		
				    	for (int k=ai1;k<=ai3;k++){
				    		d= d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
				    		if (zMap[k]>d-0.01){
				    			r[k]=d;
				    			zMap[k]=d;
				    			colorMap[k]=bgc;
				    			if      (k==ai1) corner[k]=1;
				    			else if (k==ai3) corner[k]=2;
				    			else             corner[k]=0;
				    		}
				    	}
						
					}
				}
				
				
			}
		}
		
		
		
		for (int i=0;i<180;i++){
			r2[i]= r[(i+orientationDeg-90+720)%360];
			colorMap2[i]=colorMap[(i+orientationDeg-90+720)%360];
			corner2[i]=corner[(i+orientationDeg-90+720)%360];
		}
		
		for (int i=0;i<Ernest.RESOLUTION_RETINA;i++){
			retina[Ernest.RESOLUTION_RETINA-i-1]= new EyeFixation();
			retina[Ernest.RESOLUTION_RETINA-i-1].setColor(colorMap2[(int)(i*180/Ernest.RESOLUTION_RETINA+180/Ernest.RESOLUTION_RETINA/2)]);
			retina[Ernest.RESOLUTION_RETINA-i-1].setDistance((int) r2[(int)(i*180/Ernest.RESOLUTION_RETINA+180/Ernest.RESOLUTION_RETINA/2)]);
		}
		
		if (setdistance){ 
			distance=(float) r2[90];
			frontColor=colorMap2[90];
			m_objMemory.addObject(frontColor);
		}
		
		//m_patternMap.addPatern(colorMap2,lastAction);
		//m_patternFrame.update((int)(m_x*10),(int)(m_y*10),m_orientation);
		//m_patternFrame.paint();
		
		m_int.repaint();
		eye.repaint();
		m_map.compute(r2, colorMap2);
		eye.paint(r2,colorMap2,corner2);
		//System.out.println(" distance  : "+r2[90]);
		return retina;
	}
	
	
	
	
	/**
	 * Generates a retina image from Ernest's view point.
	 * @param t The eyes direction
	 * @return The array of colors projected onto the retina.
	 */ 
	protected EyeFixation[] retina(double t)
	{
		EyeFixation[] retina = new EyeFixation[Ernest.RESOLUTION_RETINA];
		
		double angle = t - Math.PI/2 ;
		
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
		{
			retina[i] = scanArc(angle, Math.PI/Ernest.RESOLUTION_RETINA);
			angle = angle + Math.PI/Ernest.RESOLUTION_RETINA;
		}
		//time1=System.currentTimeMillis();
		//System.out.print("step : "+(time1-time2)+" millisecondes");
		rendu(false);
		//time2=System.currentTimeMillis();
		//System.out.println(" ; rendu : "+(time2-time1)+" millisecondes");
		//time2=time1;
		
		return retina;
	}
	
	/**
	 * Scan an arc from Ernest's viewpoint, starting from the initial angle position and going through the angular span.
	 * Stop scanning at the first singularity found.
	 * @param t The initial angular position (trigonometric/counterclockwise)
	 * @param a The arc's angular span (trigonometric/counterclockwise)
	 * @param d The arc's diameter (the agent's visual range)
	 * @return the color detected. 
	 */
	protected EyeFixation scanArc( double t, double a)
	{
		EyeFixation eyeFixation = new EyeFixation();
		eyeFixation.setColor(WALL_COLOR);
		eyeFixation.setColor2(WALL_COLOR);
		eyeFixation.setDistance(Ernest.INFINITE);

		double step = a/2;

		// scan the first row
		for (double angle = t ;  angle <= t + a + .001; angle += step)
		{
			int x0 =  cell(m_x) + (int) ( 20 * Math.cos(angle) + .5); // round to the closest integer 
			int y0 =  cell(m_y) - (int) ( 20 * Math.sin(angle) + .5); // Y axis is downwards
			
			eyeFixation = rayTrace(cell(m_x), cell(m_y) , x0, y0);
			
			// We stop when we find a singularity.
			if ( eyeFixation.getColor() != WALL_COLOR)
				break;
		}
		
		return eyeFixation;
	}

	/**
	 * Scan the squares that are on a ray from a viewpoint to a target square
	 * This version doesn't see the square where Ernest is standing
	 * @author ogeorgeon adapted this routine from:
	 *  http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html 
	 * @return An eye fixation that contains the color seen and its distance multiplied per 10. 
	 */
	protected EyeFixation rayTrace(float x0, float y0, int x1, int y1)
	{
		EyeFixation eyeFixation = new EyeFixation();
		eyeFixation.setColor(WALL_COLOR);
		eyeFixation.setDistance(Ernest.INFINITE);
		eyeFixation.setColor2(WALL_COLOR);
		eyeFixation.setDistance2(Ernest.INFINITE);
	    int dx = (int) Math.abs(x1 - x0);
	    int dy = (int) Math.abs(y1 - y0);
	    int x = cell(x0);
	    int y = cell(y0);
	    int n = 1 + dx + dy;
	    int x_inc = (x1 > x0) ? 1 : -1;
	    int y_inc = (y1 > y0) ? 1 : -1;
	    int error = dx - dy;
	    dx *= 2;
	    dy *= 2;
	    
	    for (; n > 0; --n)
	    {
	    	// move on along the ray
	        if (error > 0)
	        {
	            x += x_inc;
	            error -= dy;
	        }
	        else if (error < 0)
	        {
	            y += y_inc;
	            error += dx;
	        }
	        else
	        {
	            x += x_inc;
	            y += y_inc;
	            --n;
	        }

	        // Don't go outside the grid
	    	if ((x < 0) || (y < 0) || (x >= m_w) || (y >= m_h)) return eyeFixation;
	    	
	    	// Examine the square on the ray. Return wall or uninhibited dirty squares.

	    	Color bgc = getBackgroundColor(x,y);
	    	if (isWall(x,y) || isDirty(x,y))
	    	{
	    		if (isWall(x,y))
	    		{
	    			//if (isTheWallCorner(x,y))
	        			eyeFixation.setColor(bgc);
	    			//else
	        		//	eyeFixation.setColor(WALL_COLOR);
	    		}
	    		else
	    			eyeFixation.setColor(bgc);
	    		
    			eyeFixation.setDistance((int)Math.sqrt( ((x - x0)*(x - x0) + ( y - y0)*( y - y0)) * 100));
	    		return eyeFixation;
    		}
	    }
	    return eyeFixation;
	}

	
	public int[][] somatoMap()
	{
		int[][] somatoMap = new int[3][3];
		somatoMap[1][1] = soma(m_x, m_y);

		// Orientation up [0, PI/8]
		if (m_orientationAngle < ORIENTATION_UP * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[0][0] = soma(m_x - 1, m_y -1);
			somatoMap[1][0] = soma(m_x , m_y - 1);
			somatoMap[2][0] = soma(m_x + 1, m_y - 1);
			somatoMap[2][1] = soma(m_x + 1, m_y);
			somatoMap[2][2] = soma(m_x + 1, m_y + 1);
			somatoMap[1][2] = soma(m_x, m_y + 1);
			somatoMap[0][2] = soma(m_x - 1, m_y + 1);
			somatoMap[0][1] = soma(m_x - 1, m_y);			
		}
		// Orientation up right [PI/8, 3PI/8]
		else if (m_orientationAngle < ORIENTATION_UP_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[0][1] = soma(m_x - 1, m_y -1);
			somatoMap[0][0] = soma(m_x , m_y - 1);
			somatoMap[1][0] = soma(m_x + 1, m_y - 1);
			somatoMap[2][0] = soma(m_x + 1, m_y);
			somatoMap[2][1] = soma(m_x + 1, m_y + 1);
			somatoMap[2][2] = soma(m_x, m_y + 1);
			somatoMap[1][2] = soma(m_x - 1, m_y + 1);
			somatoMap[0][2] = soma(m_x - 1, m_y);			
		}
		
		// Orientation right [3PI/8, 5PI/8]]
		else if (m_orientationAngle < ORIENTATION_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[0][2] = soma(m_x - 1, m_y -1);
			somatoMap[0][1] = soma(m_x , m_y - 1);
			somatoMap[0][0] = soma(m_x + 1, m_y - 1);
			somatoMap[1][0] = soma(m_x + 1, m_y);
			somatoMap[2][0] = soma(m_x + 1, m_y + 1);
			somatoMap[2][1] = soma(m_x, m_y + 1);
			somatoMap[2][2] = soma(m_x - 1, m_y + 1);
			somatoMap[1][2] = soma(m_x - 1, m_y);			
		}
		
		// Orientation down right [5PI/8, 7PI/8]
		else if (m_orientationAngle < ORIENTATION_DOWN_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[1][2] = soma(m_x - 1, m_y -1);
			somatoMap[0][2] = soma(m_x , m_y - 1);
			somatoMap[0][1] = soma(m_x + 1, m_y - 1);
			somatoMap[0][0] = soma(m_x + 1, m_y);
			somatoMap[1][0] = soma(m_x + 1, m_y + 1);
			somatoMap[2][0] = soma(m_x, m_y + 1);
			somatoMap[2][1] = soma(m_x - 1, m_y + 1);
			somatoMap[2][2] = soma(m_x - 1, m_y);			
		}
		
		// Orientation down [7PI/8, 9PI/8]
		else if (m_orientationAngle < ORIENTATION_DOWN * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[2][2] = soma(m_x - 1, m_y -1);
			somatoMap[1][2] = soma(m_x , m_y - 1);
			somatoMap[0][2] = soma(m_x + 1, m_y - 1);
			somatoMap[0][1] = soma(m_x + 1, m_y);
			somatoMap[0][0] = soma(m_x + 1, m_y + 1);
			somatoMap[1][0] = soma(m_x, m_y + 1);
			somatoMap[2][0] = soma(m_x - 1, m_y + 1);
			somatoMap[2][1] = soma(m_x - 1, m_y);			
		}
		
		// Orientation down left [9PI/8, 11PI/8]
		else if (m_orientationAngle < ORIENTATION_DOWN_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[2][1] = soma(m_x - 1, m_y -1);
			somatoMap[2][2] = soma(m_x , m_y - 1);
			somatoMap[1][2] = soma(m_x + 1, m_y - 1);
			somatoMap[0][2] = soma(m_x + 1, m_y);
			somatoMap[0][1] = soma(m_x + 1, m_y + 1);
			somatoMap[0][0] = soma(m_x, m_y + 1);
			somatoMap[1][0] = soma(m_x - 1, m_y + 1);
			somatoMap[2][0] = soma(m_x - 1, m_y);			
		}
		
		// Orientation left [11PI/8, 13PI/8]
		else if (m_orientationAngle < ORIENTATION_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[2][0] = soma(m_x - 1, m_y -1);
			somatoMap[2][1] = soma(m_x , m_y - 1);
			somatoMap[2][2] = soma(m_x + 1, m_y - 1);
			somatoMap[1][2] = soma(m_x + 1, m_y);
			somatoMap[0][2] = soma(m_x + 1, m_y + 1);
			somatoMap[0][1] = soma(m_x, m_y + 1);
			somatoMap[0][0] = soma(m_x - 1, m_y + 1);
			somatoMap[1][0] = soma(m_x - 1, m_y);			
		}
		
		// Orientation up left [13PI/8, 15PI/8]
		else if (m_orientationAngle < ORIENTATION_UP_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
		{
			somatoMap[1][0] = soma(m_x - 1, m_y -1);
			somatoMap[2][0] = soma(m_x , m_y - 1);
			somatoMap[2][1] = soma(m_x + 1, m_y - 1);
			somatoMap[2][2] = soma(m_x + 1, m_y);
			somatoMap[1][2] = soma(m_x + 1, m_y + 1);
			somatoMap[0][2] = soma(m_x, m_y + 1);
			somatoMap[0][1] = soma(m_x - 1, m_y + 1);
			somatoMap[0][0] = soma(m_x - 1, m_y);			
		}
		// Orientation up [15PI/8, 2PI]
		else
		{
			somatoMap[0][0] = soma(m_x - 1, m_y -1);
			somatoMap[1][0] = soma(m_x , m_y - 1);
			somatoMap[2][0] = soma(m_x + 1, m_y - 1);
			somatoMap[2][1] = soma(m_x + 1, m_y);
			somatoMap[2][2] = soma(m_x + 1, m_y + 1);
			somatoMap[1][2] = soma(m_x, m_y + 1);
			somatoMap[0][2] = soma(m_x - 1, m_y + 1);
			somatoMap[0][1] = soma(m_x - 1, m_y);			
		}
		
		return somatoMap;
	}
	
	public int soma(float m_x, float m_y)
	{
		int soma = 0;
		if (m_x < 0 || m_y < 0 || m_x >= m_w || m_y >= m_h)
			return Ernest.STIMULATION_TOUCH_WALL.getValue();
		if (isDirty(cell(m_x),cell(m_y)))
		{
			if (getDirty(cell(m_x),cell(m_y)) == DIRTY)
				soma = Ernest.STIMULATION_TOUCH_FISH.getValue();
			else
				soma = Ernest.STIMULATION_TOUCH_SOFT.getValue();
		}
		if (isWall(cell(m_x),cell(m_y))) soma = Ernest.STIMULATION_TOUCH_WALL.getValue();
		return soma;
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
		
		EyeFixation[] eyeFixation = null;
		Color[][] pixelColor = new Color[Ernest.RESOLUTION_RETINA][2];
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
			pixelColor[i][0] = UNANIMATED_COLOR;
		
		// body Color
		
		Color[][] somatoMapColor = new Color[3][3];
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				somatoMapColor[i][j] = UNANIMATED_COLOR;
		
		// Animate Ernest when he is alive

		if (m_ernest != null)
		{
			// Eye color
			//eyeFixation = retina(Math.PI/2 - m_orientationAngle);
			eyeFixation=rendu(false);
			for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
			{
				pixelColor[i][0] = eyeFixation[i].getColor();
				pixelColor[i][1] = eyeFixation[i].getColor2();
			}
			
			// Somatomap color
			int [][] somatoMap = somatoMap();
			for (int i = 0; i < 3; i++)
				for (int j = 0; j < 3; j++)
				{
					if (somatoMap[i][j] == Ernest.STIMULATION_TOUCH_EMPTY.getValue())
						somatoMapColor[i][j] = new Color(Ernest.COLOR_TOUCH_EMPTY.getRGB());//Ernest.COLOR_TOUCH_EMPTY;
					if (somatoMap[i][j] == Ernest.STIMULATION_TOUCH_SOFT.getValue())
						somatoMapColor[i][j] = new Color(Ernest.COLOR_TOUCH_ALGA.getRGB());
					if (somatoMap[i][j] == Ernest.STIMULATION_TOUCH_WALL.getValue())
						somatoMapColor[i][j] = new Color(Ernest.COLOR_TOUCH_WALL.getRGB());
					if (somatoMap[i][j] == Ernest.STIMULATION_TOUCH_FISH.getValue())
						somatoMapColor[i][j] = new Color(Ernest.COLOR_TOUCH_FISH.getRGB());
				}
		}
		
		// The shark body

		Area sharkMask = sharkmask();
		
		// Retina pixel
		
		Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
		Arc2D.Double pixelOut = new Arc2D.Double(-25, -25, 50, 50,0, 180 / Ernest.RESOLUTION_RETINA + 1 , Arc2D.PIE);
		
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
		g2d.setColor(getBackgroundColor(cell(m_x), cell(m_y)));
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
			g2d.setColor(pixelColor[i][0]);
			g2d.fill(pixelIn);
			g2d.transform(transformSegment);
		}
	}

	/**
	 * Paint Ernest's obervation.
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
		int span = 1;
		
		// body Color
		
		Color[][] somatoMapColor = new Color[3][3];
		for (int i = 0; i < 3; i++)
			for (int j = 0; j < 3; j++)
				somatoMapColor[i][j] = Color.WHITE;
		
		// Animate Ernest when he is alive

		if (m_ernest.size()!=0 && m_ernest.get(0) != null && m_ernest.get(0).getObservation() != null)
		{
			// Eye
			if (m_ernest.get(0).getObservation().getSalience() != null)
			{
				span = m_ernest.get(0).getObservation().getSalience().getSpan();
				direction = m_ernest.get(0).getObservation().getSalience().getDirection() / 10f - span / 2f + .5f;
				eyeColor = new Color(m_ernest.get(0).getObservation().getSalience().getColor().getRGB());
			}
						
			// Somatomap color
			for (int i = 0; i < 3; i++)
				for (int j = 0; j < 3; j++)
					somatoMapColor[i][j] = new Color(m_ernest.get(0).getObservation().getColor(i, j).getRGB());
			
//			if (m_ernest.getObservation().getLabel() == null)
//				System.out.println("Observation has no label");
			if (Ernest.STIMULATION_KINEMATIC_BUMP.equals(m_ernest.get(0).getObservation().getKinematic()))
				kinematicColor = Color.RED;
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

		g2d.setColor(new Color(Ernest.COLOR_WALL.getRGB()));
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
		
		Area sharkMask = new Area(new Rectangle2D.Double(-50, -50, 100, 100));
		sharkMask.subtract(shark);

		return sharkMask;
	}



	// save the actual ernest
	public void save(){
		System.out.println("=====================prepare to save...======================");
		String fileName = "/home/simon/Bureau/Ernest.txt";
		
		try {
			PrintWriter file  = new PrintWriter(new FileWriter(fileName));
			
			// Ernest name
			file.println("Ernest");
			file.println();
			
			// actions matrix
			for (int act=0;act<m_actionList.size();act++){
				// new action, set name and size
				int o_width =m_actionList.get(act).width/5;
				int o_height=m_actionList.get(act).height/5;
				file.println("Action "+m_actionList.get(act).name+" "+o_width+" "+o_height);
				
				// for each object of the action, set number and matrix
				for (int o=0;o<m_actionList.get(act).selectMap.size();o++){
					file.println("selectMap "+o);
					for (int i=0;i<o_width;i++){
						for (int j=0;j<o_height;j++){
							file.print(m_actionList.get(act).selectMap.get(o)[i][j]+" ");
						}
						file.println();
					}
					file.println("confidenceMap "+o);
					for (int i=0;i<o_width;i++){
						for (int j=0;j<o_height;j++){
							file.print(m_actionList.get(act).confidenceMap.get(o)[i][j]+" ");
						}
						file.println();
					}
				}
			}
			
			// object list
			for (int i=0;i<m_objMemory.objectList.size();i++){
				file.println("object "+m_objMemory.objectList.get(i).getRed()  +" "+
									   m_objMemory.objectList.get(i).getGreen()+" "+
									   m_objMemory.objectList.get(i).getBlue() +" "+
									   m_objMemory.value.get(i));
			}

			
			// primitive Schemas
			/*int nb=m_ernest.get(0).getEpisodicMemory().m_schemas.size();
			for (int i=0;i<nb;i++){
				if (m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).isPrimitive()){
					file.println("schema "+1+" "+m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getId()+" "+
											     m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getLabel()+" "+
											     m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getWeight());
				}
			}*/
			
			
			file.close();
			
			System.out.println("file saved");

		} catch (Exception e) {
			System.out.print("Erreur : ");
			e.printStackTrace();
		}


	}

	// load a ernest file
	public boolean load(ArrayList<Action> actList){
		
		String fileName = "/home/simon/Bureau/Ernest.txt";
		
		boolean succes=true;
		int nbLine=0;
		int nbAct=0;
		int nbObj=0;
		int indexObj=0;
		int matrixType=0;      // 1->selectMap, 2->confidenceMap
		int indexLine=0;
		int h=0;
		
		try {
			Scanner scanner=new Scanner(new File(fileName));
			
			String[] elements;
			
			while (scanner.hasNextLine() && succes) {
			    String line = scanner.nextLine();
			    
			    if (nbLine==0) System.out.println(line);
			    
			    if (nbLine>=2){
			    	elements=line.split(" ");
			    	if (elements.length>0){
			    		// new non empty line

			    		// case new Action
			    		if (elements[0].equals("Action")){
			    			if (elements.length == 4){
			    				actList.add(new Action(elements[1],10,Integer.parseInt(elements[2])*5,Integer.parseInt(elements[3])*5,m_objMemory));
			    				nbAct++;
			    				nbObj=0;
			    				indexObj=0;
			    				indexLine=0;
			    				h=Integer.parseInt(elements[3]);
			    			}
			    			else succes=false;
			    		}
			    		
			    		// case selectMap
			    		else if (elements[0].equals("selectMap")){
			    			if (elements.length == 2 && nbAct>0){
			    				if (Integer.parseInt(elements[1])>=nbObj){
			    					while (Integer.parseInt(elements[1])>=nbObj){
				    					actList.get(nbAct-1).addObject();
				    					nbObj++;	
			    					}
			    				}
			    				indexObj=Integer.parseInt(elements[1]);
			    				matrixType=1;
			    				indexLine=0;
			    			}
			    			else succes=false;
			    		}
			    		
			    		
			    		// case confidenceMap
			    		else if (elements[0].equals("confidenceMap")){
			    			if (elements.length == 2 && nbAct>0){
			    				if (Integer.parseInt(elements[1])>nbObj){
			    					while (Integer.parseInt(elements[1])>=nbObj){
				    					actList.get(nbAct-1).addObject();
				    					nbObj++;	
			    					}
			    				}
			    				indexObj=Integer.parseInt(elements[1]);
			    				matrixType=2;
			    				indexLine=0;
			    			}
			    			else succes=false;
			    		}
			    		
			    		
			    		// case object
			    		else if (elements[0].equals("object")){
			    			if (elements.length == 5){
			    				
			    				if (!elements[4].equals("null")) m_objMemory.loadObject(new Color(Integer.parseInt(elements[1]) ,
			    												    							 Integer.parseInt(elements[2]) ,
			    												    							 Integer.parseInt(elements[3]) ),
			    												    							 Float.parseFloat(elements[4]));
			    				else 	m_objMemory.addObject(new Color(Integer.parseInt(elements[1]) ,
		    							 								Integer.parseInt(elements[2]) ,
		    							 								Integer.parseInt(elements[3]) ));

			    				
			    				
			    				for (int j=0;j<actList.size();j++){
			    					actList.get(j).distances.add(new ArrayList<Float>());
			    					for(int k=0;k<m_objMemory.objectList.size();k++){
			    						actList.get(j).distances.get(actList.get(j).distances.size()-1).add((float) -1);
			    					}
			    					for (int k=0;k<m_objMemory.objectList.size()-1;k++){
			    						actList.get(j).distances.get(k).add((float) -1);
			    					}
					    			
			    				}
			    			}
			    			
			    		}
			    		
			    		
			    		// schemas
			    		else if (elements[0].equals("schema")){
			    			// case simple schema
			    			/*if (elements.length > 2 && elements[1].equals("1") && elements.length==5){
			    				m_ernest.get(0).getEpisodicMemory().m_schemas.add( new Schema( Integer.parseInt(elements[2]),
			    						                                                       elements[3],
			    						                                                       Integer.parseInt(elements[4])) );
			    			}*/
			    		}
			    		
			    		// matrix value (no label)
			    		else{
			    			if (indexLine<actList.get(nbAct-1).width){
			    			int min=Math.min(h, elements.length);
			    			for (int j=0;j<min;j++){
			    				if (matrixType==1) actList.get(nbAct-1).selectMap.get(indexObj)[indexLine][j]=Float.parseFloat(elements[j]);
			    				if (matrixType==2) actList.get(nbAct-1).confidenceMap.get(indexObj)[indexLine][j]=Float.parseFloat(elements[j]);   					
			    			}
			   				indexLine++;
			    			};
			    		}
			    		
		
			    	}
			    	else succes=false;			    	
			    }	 
			    nbLine++;
			}
			
			
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}

		if (nbLine<3) succes=false;
		
		for (int k=0;k<actList.size();k++){
			for (int i=0;i<m_objMemory.objectList.size();i++){
				for (int j=0;j<m_objMemory.objectList.size();j++){
					
					if (i==j) actList.get(k).distances.get(i).set(j, (float) 0);
					else      actList.get(k).distances.get(i).set(j, actList.get(k).distance(m_objMemory,
																							 m_objMemory.objectList.get(i),
																							 m_objMemory.objectList.get(j)));
					
				}
			}
		}
		
		
		return succes;
	}

}