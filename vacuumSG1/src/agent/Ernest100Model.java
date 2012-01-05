package agent;


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

import javax.vecmath.Vector3f;

import ernest.*;
import tracing.*;
import memory.*;


/**************************************
 * A Model for Ernest 10
 * Ernest can turn PI/4 and move in diagonal.
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
	
	//private long time1=System.currentTimeMillis();
	//private long time2=System.currentTimeMillis();
	
	public int lastAction;
	
	public Main mainFrame;
	
	//public String intention;
	public boolean status;
	public boolean acting;
	
	public TactileMap m_tactile;
	public TactileMapFrame m_tactileFrame;
	
	public VisualMap m_visual;
	public VisualMapFrame m_visualFrame;
	
	public Colliculus colliculus;
	public ColliculusFrame colliculusFrame;
	
	private EyeView eye;
	
	//public Color frontColor;
	
	public boolean tempo=true;
	
	public Ernest100Model(int i) {
		super(i);
	}
	
	/**
	 * Initialize the agent in the grid
	 */
	public void init(int w,int h) throws Exception
	{
		// Initialize the model
		super.init(w,h);
		setEyeAngle(Math.PI/4);
		setOrientationStep(45);

		setChanged();
		notifyObservers2();

		if (m_tactile==null){
			m_tactile=new TactileMap(this);
		}
		if (m_visual==null){
			m_visual=new VisualMap(this);
		}
		
		colliculus=new Colliculus(m_tactile,m_visual);
		//colliculusFrame=new ColliculusFrame(colliculus);
	}

	/**
	 * @return The version of the Ernest model
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
		m_ernest = new Ernest();
		
		m_sensorymotorSystem = new Visual100SensorymotorSystem();
		//m_tracer = new XMLTracer("trace.xml");
		//m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","pDCJHmOykTgkgyZbKVtHFEtS-PujoS");
		
		// Initialize the Ernest === 
		
		m_ernest.setParameters(5, 4);
		m_ernest.setTracer(m_tracer);
		m_ernest.setSensorymotorSystem(m_sensorymotorSystem);
		


		// Ernest's inborn primitive interactions
		
			//m_ernest.addInteraction(">", " ",   20); // Move
			//m_ernest.addInteraction(">", "w", -100); // Bump 
			
			//m_ernest.addInteraction("^", " ",  -10); // Left toward empty
			//m_ernest.addInteraction("^", "w",  -20); // Left toward wall
	
			//m_ernest.addInteraction("v", " ",  -10); // Right toward empty
			//m_ernest.addInteraction("v", "w",  -20); // Right toward wall
			//m_sensorymotorSystem.addPrimitiveAct("^", true,   -50); // Left toward empty
			//m_sensorymotorSystem.addPrimitiveAct("^", false,  -70); // Left toward wall

			//m_sensorymotorSystem.addPrimitiveAct("v", true,   -50); // Right toward empty
			//m_sensorymotorSystem.addPrimitiveAct("v", false,  -70); // Right toward wall

		m_ernest.addInteraction(">", " ",   20); // Move
			
		m_ernest.addInteraction("^", " ",  -10); // Left toward empty
	
		m_ernest.addInteraction("v", " ",  -10); // Right toward empty

//			m_sensorymotorSystem.addPrimitiveAct(">", true,   100); // Move
//			m_sensorymotorSystem.addPrimitiveAct(">", false, -100); // Bump 
//			m_sensorymotorSystem.addPrimitiveAct("^", true,   -10); // Left toward empty
//			m_sensorymotorSystem.addPrimitiveAct("^", false,  -20); // Left toward wall
//
//			m_sensorymotorSystem.addPrimitiveAct("v", true,   -10); // Right toward empty
//			m_sensorymotorSystem.addPrimitiveAct("v", false,  -20); // Right toward wall

		System.out.println("Ernest initialized") ;
	}

	
	public void setFrame(Main m){
		mainFrame=m;
	}
	
	/**
	 * Initialize the agent's parameters
	 */
	protected void initAgent()
	{
	}
	
	public void setDisplay(){
		
		//////////////////////
		int size=m_env.frameList.size();
		int i=0;
		boolean found=false; 
		while (i<size && !found){
			if (m_env.frameList.get(i).getClass().getName().equals("memory.TactileMapFrame")) found=true;
			i++;
		}
		
		if (!found) m_env.frameList.add(new TactileMapFrame(m_tactile)); 
		else        ((TactileMapFrame) m_env.frameList.get(i-1)).setTactile(m_tactile);
		
		////////////////////
		size=m_env.frameList.size();
		i=0;
		found=false; 
		while (i<size && !found){
			if (m_env.frameList.get(i).getClass().getName().equals("memory.VisualMapFrame")) found=true;
			i++;
		}
		
		if (!found) m_env.frameList.add(new VisualMapFrame(m_visual)); 
		else        ((VisualMapFrame) m_env.frameList.get(i-1)).setVisual(m_visual);
		
		///////////////////
		size=m_env.frameList.size();
		i=0;
		found=false; 
		while (i<size && !found){
			if (m_env.frameList.get(i).getClass().getName().equals("memory.ColliculusFrame")) found=true;
			i++;
		}
		
		if (!found) m_env.frameList.add(new ColliculusFrame(colliculus)); 
		else        ((ColliculusFrame) m_env.frameList.get(i-1)).setColliculus(colliculus);
		
	}

	public void update(){
		float vlmin=0.1f;
		float vrmin=0.002f;
		
		if ( !((mTranslation.length()>vlmin) ||  (mRotation.length()>vrmin)) ){
			int[] intention = stepErnest(status);
			enactSchema(intention);
		}

		isStep=true;
		
		status = impulse(lastAction);
		
		if ( !((mTranslation.length()>vlmin) ||  (mRotation.length()>vrmin)) ) isStep=false;
	}
	
	/**
	 * Run Ernest one step
	 */
	public int[] stepErnest(boolean status)
	{
		if (m_tracer != null)
			m_tracer.startNewEvent(m_counter);

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
			matrix[i][5] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getRed();
			matrix[i][6] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getGreen();
			matrix[i][7] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getBlue();
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
//		for (int i = 0; i < 3; i++)
//			for (int j = 0; j < 3; j++)
//				matrix[i][9 + j] = somatoMap[i][j];
		for (int i = 0; i < 9; i++)
			matrix[i][9] = somatoMap[i];
		
		// Circadian (information on day or night)
		
		matrix[2][8] = (isNight() ? 1 : 0);		
		
		// The salience list.
		m_ernest.setPlaceList(colliculus.getPlaceList());

		//String intention = m_ernest.step(matrix);
		String intention = Character.toString((char)m_ernest.step(matrix)[0]);
		//return intention;
		return m_ernest.step(matrix);
	}
	
	/**
	 * Enact the primitive schema chosen by Ernest.
	 * @return binary feedback. 
	 */
	public boolean enactSchema(int[] schema)
	{
		//m_schema = schema;
		m_schema = Character.toString((char)schema[0]);
		
		// A new interaction cycle is starting
		m_counter++;
		System.out.println("Agent #"+ident+", Step #" + m_counter + "=======");
		
		boolean status = true;
		
		if (isNight())
		{
			setChanged();
			notifyObservers2();			
			sleep(100);
		}
		else
		{
			if (m_schema.equals(""))
			{
				setChanged();
				notifyObservers2();			
				//if (tempo) sleep(10);
				status = true;
			}
			else if (m_schema.equals("v"))
				status = turnRight();
			else if (m_schema.equals("^"))
				status = turnLeft();
			else if (m_schema.equals(">"))
				status = forward();
	
			// Trace the environmental data
			if (m_tracer != null)
			{
				Object environment = m_tracer.newEvent("environment", "position", m_counter);
				m_tracer.addSubelement(environment, "x", mPosition.x + "");
				m_tracer.addSubelement(environment, "y", mPosition.y + "");
				m_tracer.addSubelement(environment,"orientation", mOrientation.z + "");
			}
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
		if (m_env.isFood(mPosition.x,mPosition.y))
			suck();

		int stimulation = ((m_env.isFood(mPosition.x,mPosition.y)) ? Ernest.STIMULATION_GUSTATORY_FISH : Ernest.STIMULATION_GUSTATORY_NOTHING);
		return stimulation;
	}
	
	/**
	 * Turn left. 
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnLeft(){
		
		mRotation.z=(float) (Math.PI/4);
		mTranslation.scale(0);
		
		lastAction=ACTION_LEFT;
		return status;
	}
	
	/**
	 * Turn right.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean turnRight(){

		mRotation.z=(float)(-Math.PI/4);
		mTranslation.scale(0);
		
		lastAction=ACTION_RIGHT;
		return status;
	}
	
	/**
	 * Move forward.
	 * @return true if adjacent wall, false if adjacent empty. 
	 */
	protected boolean forward(){

		mTranslation.x=1.5f;
		mTranslation.y=0;
		mRotation.scale(0);
		
		lastAction=ACTION_FORWARD;
		return status;
	}

	
	/**
	 * move Ernest with an impulsion
	 * @param act Action to perform
	 * @return result of the interaction
	 */
	public boolean impulse(int act){
		boolean statusL=true;
		boolean statusR=true;
		float stepX,stepY;                  // length of a step
		float HBradius=(float) 0.4;  // radius of Ernest hitbox 
		
		int cell_x=Math.round(mPosition.x);
		int cell_y=Math.round(mPosition.y);
		
		boolean status1=true;         // vertical motion
		boolean status2=true;         // horizontal motion
		boolean status3=true;         // begin on a dirty cell
		boolean status4=true;         // reach a dirty cell (=false when reach dirty cell)
		boolean status5=true;         // touch a corner

		float orientation=mOrientation.z;
		
		float vlmin=0.1f;
		float vrmin=0.002f;
		
		status3=m_env.isAlga(cell_x,cell_y) || m_env.isFood(cell_x,cell_y);
		
			
			
			
	// compute new position
			
			// for linear movements
			double d;
			if (statusL){
				stepX=mTranslation.x/30;
				stepY=mTranslation.y/30;
				
				double dx= stepX*Math.cos(mOrientation.z) - stepY*Math.sin(mOrientation.z);
				double dy= stepX*Math.sin(mOrientation.z) + stepY*Math.cos(mOrientation.z);
				cell_x=Math.round(mPosition.x);
				cell_y=Math.round(mPosition.y);
				mPosition.x+=dx;
				mPosition.y+=dy;
			}
			
			// for angular movements
			orientation+=mRotation.z/10;

			if (orientation <= -Math.PI) orientation +=2*Math.PI;
			if (orientation >   Math.PI) orientation -=2*Math.PI;

			mOrientation.z = orientation;
			
			
	// compute state
		// for linear movement
			// current cell
			if (m_env.isAlga(cell_x,cell_y) || m_env.isFood(cell_x,cell_y)){
				if (status3 && !m_env.isAlga(cell_x,cell_y) && !m_env.isFood(cell_x,cell_y) ) status3=false;
				if (!status3 && (m_env.isAlga(cell_x,cell_y) || m_env.isFood(cell_x,cell_y))) status4=false;
			}
			// bottom cell
			if ( (m_env.isWall(cell_x,cell_y-1)) && (mPosition.y-HBradius) -((float)cell_y-0.5)<0 ){
				status1=false;
				mPosition.y+= ((float)cell_y-0.5) - (mPosition.y-HBradius);
			}
			// right cell
			if ( (m_env.isWall(cell_x+1,cell_y)) && ((float)cell_x+0.5) -(mPosition.x+HBradius)<0 ){
				status2=false;
				mPosition.x-= (mPosition.x+HBradius) - ((float)cell_x+0.5);
			}
			// top cell
			if ( (m_env.isWall(cell_x,cell_y+1)) && ((float)cell_y+0.5) -(mPosition.y+HBradius)<0 ){
				status1=false;
				mPosition.y-= (mPosition.y+HBradius) - ((float)cell_y+0.5);
			}
			// left cell
			if ( (m_env.isWall(cell_x-1,cell_y)) && (mPosition.x-HBradius) -((float)cell_x-0.5)<0 ){
				status2=false;
				mPosition.x+= ((float)cell_x-1+0.5) - (mPosition.x-HBradius);
			}
			// bottom right
			d= (mPosition.x-(cell_x+1-0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
			d=Math.sqrt(d);
			if (m_env.isWall(cell_x+1,cell_y-1) && d-0.4<0){
				while (d-0.4<0){
					mPosition.x-=0.01;
					mPosition.y+=0.01;
					d= (mPosition.x-(cell_x+0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// top right
			d= (mPosition.x-(cell_x+0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y+0.5))*(mPosition.y-(cell_y+0.5));
			d=Math.sqrt(d);
			if (m_env.isWall(cell_x+1,cell_y+1) && d-0.4<0){
				while (d-0.4<0){
					mPosition.x-=0.01;
					mPosition.y-=0.01;
					d= (mPosition.x-(cell_x+0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// top left
			d= (mPosition.x-(cell_x-0.5))*(mPosition.x-(cell_x-0.5))+(mPosition.y-(cell_y+0.5))*(mPosition.y-(cell_y+0.5));
			d=Math.sqrt(d);
			if (m_env.isWall(cell_x-1,cell_y+1) && d-0.4<0){
				while (d-0.4<0){
					mPosition.x+=0.01;
					mPosition.y-=0.01;
					d= (mPosition.x-(cell_x+0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			// bottom left
			d= (mPosition.x-(cell_x-0.5))*(mPosition.x-(cell_x-0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
			d=Math.sqrt(d);
			if (m_env.isWall(cell_x-1,cell_y-1) && d-0.4<0){
				while (d-0.4<0){
					mPosition.x+=0.01;
					mPosition.y+=0.01;
					d= (mPosition.x-(cell_x+0.5))*(mPosition.x-(cell_x+0.5))+(mPosition.y-(cell_y-0.5))*(mPosition.y-(cell_y-0.5));
					d=Math.sqrt(d);
				}
				status5=false;
			}
			
			// other agents
			for (int a=0;a<m_env.m_modelList.size();a++){
				if (a!=ident){
					float dx=m_env.m_modelList.get(a).mPosition.x-mPosition.x;
					float dy=m_env.m_modelList.get(a).mPosition.y-mPosition.y;
					d= (dx*dx)+(dy*dy);
					d=Math.sqrt(d);
					
					
					
					if (d<0.8){
						
						float dx2=(float) ( (dx/d) * (0.8-d)/d);
						float dy2=(float) ( (dy/d) * (0.8-d)/d);
						
						//mPosition.x-=dx2/2;
						//mPosition.y-=dy2/2;
							
						if (m_env.m_modelList.get(a).run || m_env.m_modelList.get(a).step){
							m_env.m_modelList.get(a).mPosition.x+=dx2/2;
							m_env.m_modelList.get(a).mPosition.y+=dy2/2;
						}
						else{
							mPosition.x-=dx2/2;
							mPosition.y-=dy2/2;
						}
						
						/*
						double dx2= Math.cos(-mOrientation.z) - Math.sin(-mOrientation.z);
						double dy2= Math.sin(-mOrientation.z) + Math.cos(-mOrientation.z);
						
						System.out.println("----------- "+ident+" ; "+dx2+" , "+dy2);
						
						mTranslation.x=(float) (mTranslation.x*(1-dx2));
						mTranslation.y=(float) (mTranslation.y*(1-dy2));
						/*
						dx2= Math.cos(-m_env.m_modelList.get(a).mOrientation.z) - Math.sin(-m_env.m_modelList.get(a).mOrientation.z);
						dy2= Math.sin(-m_env.m_modelList.get(a).mOrientation.z) + Math.cos(-m_env.m_modelList.get(a).mOrientation.z);
						
						m_env.m_modelList.get(a).mTranslation.x=(float) (m_env.m_modelList.get(a).mTranslation.x*(1-dx2));
						m_env.m_modelList.get(a).mTranslation.y=(float) (m_env.m_modelList.get(a).mTranslation.y*(1-dy2));
						*/
					}
				}
			}
			
			/*
			if (tempo){

				//mainFrame.drawGrid();
				//m_int.repaint();
				//sleep((int)(1));
			}*/
			
			if (display){
				
				for (int i=0;i<m_env.frameList.size();i++){
					m_env.frameList.get(i).repaint();
				}
			}
			
			
			statusL=status1 && status2 && status4;
			
			float speed=0;
			
			if (lastAction==0) speed= mTranslation.length();
			if (lastAction==1) speed=-mRotation.z;
			if (lastAction==2) speed= mRotation.z;
			
			if (lastAction==0 && (!statusL && !status5) ) speed=0; 
			
			rendu(true,speed);
			
			//if (!statusL) mTranslation.scale(0);
			
			// set linear impulsion
			mTranslation.scale(0.97f);
		
			// set angular impulsion
			mRotation.scale(0.9f);
		
		
	// compute state for angular movement
		int adjacent_x = Math.round(mPosition.x);
		int adjacent_y = Math.round(mPosition.y);
		
		// Adjacent square
		if (mOrientation.z >= 7*Math.PI/8 && mOrientation.z < -7*Math.PI/8)
			adjacent_x = Math.round(mPosition.x) - 1;
		if (mOrientation.z <=  7*Math.PI/8 && mOrientation.z > 5*Math.PI/8){
			adjacent_x = Math.round(mPosition.x) - 1;
			adjacent_y = Math.round(mPosition.y) + 1;
		}
		if (mOrientation.z <=  5*Math.PI/8 && mOrientation.z > 3*Math.PI/8)
			adjacent_y = Math.round(mPosition.y) + 1;
		if (mOrientation.z <=  3*Math.PI/8 && mOrientation.z >   Math.PI/8){
			adjacent_x = Math.round(mPosition.x) + 1;
			adjacent_y = Math.round(mPosition.y) + 1;
		}
		if (mOrientation.z <=    Math.PI/8 && mOrientation.z >  -Math.PI/8)
			adjacent_x = Math.round(mPosition.x) + 1;
		if (mOrientation.z <=   -Math.PI/8 && mOrientation.z >-3*Math.PI/8){
			adjacent_x = Math.round(mPosition.x) + 1;
			adjacent_y = Math.round(mPosition.y) - 1;
		}
		if (mOrientation.z <= -3*Math.PI/8 && mOrientation.z >-5*Math.PI/8)
			adjacent_y = Math.round(mPosition.y) - 1;
		if (mOrientation.z <= -5*Math.PI/8 && mOrientation.z >-7*Math.PI/8){
			adjacent_x = Math.round(mPosition.x) - 1;
			adjacent_y = Math.round(mPosition.y) - 1;
		}

		
		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h)){
			if (m_env.isWall(adjacent_x, adjacent_y)){
				setAnim(adjacent_x,adjacent_y, ANIM_RUB); 
				statusR = false;
			}
		}
		else
			statusR = false;

		if ((adjacent_x >= 0) && (adjacent_x < m_w) && (adjacent_y >= 0) && (adjacent_y < m_h))
			setAnim(adjacent_x, adjacent_y, ANIM_NO);	
		
		//setChanged();
		//notifyObservers2();			
		
		//if (tempo) sleep(10);
		
		//setChanged();
		//notifyObservers2();
		
		
		rendu(true);
		
		if (act==0) return status1 && status2; // && status5;
		else        return statusR;
	}
	
	
	protected EyeFixation[] rendu(boolean setdistance){
		return rendu(false,0);
	}
	
	
	//******************************************
	////////////////////////////////////////////
	//******************************************
	protected EyeFixation[] rendu(boolean sensor,float speed){
		double[] rv    = new double[360];
		double[] rv2   = new double[360];
		double[] rt    = new double[360];
		double[] rt2   = new double[360];
		
		double[] zVMap = new double[360];
		double[] zTMap = new double[360];
		
		Color[] colorMap =new Color[360];
		Color[] colorMap2=new Color[360];
		int[] tactileMap =new int[360];
		int[] tactileMap2=new int[360];
		
		int[] cornerV = new int[360];
		int[] cornerV2= new int[360];
		int[] cornerT = new int[360];
		int[] cornerT2= new int[360];
		
		EyeFixation[] retina= new EyeFixation[Ernest.RESOLUTION_RETINA];
		
		double d=0;
		double d1,d2,d3,d4;
		double a1,a2,a3,a4;
		
		double imin,iplus,jmin,jplus;
		double imin2,jmin2;
		
		int Im_x=Math.round(mPosition.x);
		int Im_y=Math.round(mPosition.y);
		
		for (int i=0;i<360;i++){
			zVMap[i]=1000;
			zTMap[i]=1000;
			rv[i]=200;
			rt[i]=200;
			colorMap[i]=new Color(0,0,0);
			tactileMap[i]=0;
		}
		
		int sight=20;
		int orientationDeg= (int)(mOrientation.z * 180 / Math.PI);
		
		
		for (int i=0;i<sight;i++){
			for (int j=0;j<sight;j++){
				
				// cells on the top right side
				if ( (i>0)&& (Im_x+i<m_w) && (Im_y+j<m_h) ){
					if (m_env.isWall(Im_x+i,Im_y+j) || m_env.isAlga(Im_x+i,Im_y+j) || m_env.isFood(Im_x+i,Im_y+j) ){
						Color bgc = m_env.m_blocks[Im_x+i][Im_y+j].seeBlock();
						int tactile=m_env.m_blocks[Im_x+i][Im_y+j].touchBlock();
						
						imin =(double)i-0.5 - (mPosition.x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 - (mPosition.x-Im_x);
						jmin =(double)j-0.5 - (mPosition.y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 - (mPosition.y-Im_y);
						
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
							if (m_env.m_blocks[Im_x+i][Im_y+j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]= d;
									colorMap[k]=bgc;
									if      (k==ai2) cornerV[k]=1;
									else if (k==ai1) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]= d;
								tactileMap[k]=tactile;
								if      (k==ai2) cornerT[k]=1;
								else if (k==ai1) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d= d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
							if (m_env.m_blocks[Im_x+i][Im_y+j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]= d;
									colorMap[k]=bgc;
									if      (k==ai1) cornerV[k]=1;
									else if (k==ai3) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]= d;
								tactileMap[k]=tactile;
								if      (k==ai1) cornerT[k]=1;
								else if (k==ai3) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}
						
					}
				}

				// cells on the bottom right side
				if ( (j>0) && (Im_x+i<m_w) && (Im_y-j>=0) ){
					if (m_env.isWall(Im_x+i,Im_y-j) || m_env.isAlga(Im_x+i,Im_y-j) || m_env.isFood(Im_x+i,Im_y-j) ){
						Color bgc = m_env.m_blocks[Im_x+i][Im_y-j].seeBlock();
						int tactile=m_env.m_blocks[Im_x+i][Im_y-j].touchBlock();
						
						
						
						imin =(double)i-0.5 - (mPosition.x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 - (mPosition.x-Im_x);
						jmin =(double)j-0.5 + (mPosition.y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 + (mPosition.y-Im_y);
						
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
							if (m_env.m_blocks[Im_x+i][Im_y-j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]= d;
									colorMap[k]=bgc;
								
									if      (k==ai2) cornerV[k]=1;
									else if (k==ai1) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]= d;
								tactileMap[k]=tactile;
							
								if      (k==ai2) cornerT[k]=1;
								else if (k==ai1) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d= ( d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1));
							if (m_env.m_blocks[Im_x+i][Im_y-j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]= d;
									colorMap[k]=bgc;
									if      (k==ai1) cornerV[k]=1;
									else if (k==ai3) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]= d;
								tactileMap[k]=tactile;
							
								if      (k==ai1) cornerT[k]=1;
								else if (k==ai3) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}
					}
				}
				
				// cells on the bottom left side
				if ( (i>0) && (Im_x-i>=0) && (Im_y-j>=0) ){
					if (m_env.isWall(Im_x-i,Im_y-j) || m_env.isAlga(Im_x-i,Im_y-j) || m_env.isFood(Im_x-i,Im_y-j) ){
						Color bgc = m_env.m_blocks[Im_x-i][Im_y-j].seeBlock();
						int tactile=m_env.m_blocks[Im_x-i][Im_y-j].touchBlock();
						
						imin =(double)i-0.5 + (mPosition.x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (mPosition.x-Im_x);
						jmin =(double)j-0.5 + (mPosition.y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 + (mPosition.y-Im_y);
						
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
							if (m_env.m_blocks[Im_x-i][Im_y-j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]=d;
									colorMap[k]=bgc;
									if      (k==ai2) cornerV[k]=1;
									else if (k==ai1) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]=d;
								tactileMap[k]=tactile;
								if      (k==ai2) cornerT[k]=1;
								else if (k==ai1) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}		
						for (int k=ai1;k<=ai3;k++){
							d=  d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
							if (m_env.m_blocks[Im_x-i][Im_y-j].isVisible()){
								if (zVMap[k]>d){
									rv[k]=d;
									zVMap[k]=d;
									colorMap[k]=bgc;
									if      (k==ai1) cornerV[k]=1;
									else if (k==ai3) cornerV[k]=2;
									else             cornerV[k]=0;
								}
							}
							if (zTMap[k]>d){
								rt[k]=d;
								zTMap[k]=d;
								tactileMap[k]=tactile;
								if      (k==ai1) cornerT[k]=1;
								else if (k==ai3) cornerT[k]=2;
								else             cornerT[k]=0;
							}
						}
						
					}
				}
				
				// cells exactly on the top
				if ( (j>0) && (i==0) && (Im_y+j<m_h) ){
					if (m_env.isWall(Im_x-i,Im_y+j) || m_env.isAlga(Im_x-i,Im_y+j) || m_env.isFood(Im_x-i,Im_y+j) ){
						Color bgc = m_env.m_blocks[Im_x-i][Im_y+j].seeBlock();
						int tactile=m_env.m_blocks[Im_x-i][Im_y+j].touchBlock();
						
						imin =(double)i-0.5 + (mPosition.x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (mPosition.x-Im_x);
						jmin =(double)j-0.5 - (mPosition.y-Im_y);
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
				    		if (m_env.m_blocks[Im_x-i][Im_y+j].isVisible()){
				    			if (zVMap[k]>d){
				    				rv[k]=d;
				    				zVMap[k]= d;
				    				colorMap[k]=bgc;
				    				if      (k==ai2) cornerV[k]=1;
				    				else             cornerV[k]=0;
				    			}
				    		}
				    		if (zTMap[k]>d){
			    				rt[k]=d;
			    				zTMap[k]= d;
			    				tactileMap[k]=tactile;
			    				if      (k==ai2) cornerT[k]=1;
			    				else             cornerT[k]=0;
			    			}
				    		count++;
				    	}
				    	for (int k=0;k<=ai1;k++){
				    		d= d2*10 +   (d1-d2)*10*(k+count)/((ai1-ai2+360)%360);
				    		if (m_env.m_blocks[Im_x-i][Im_y+j].isVisible()){
				    			if (zVMap[k]>d){
				    				rv[k]=d;
				    				zVMap[k]= d;
				    				colorMap[k]=bgc;
				    				if (k==ai1) cornerV[k]=2;
				    				else        cornerV[k]=0;	
				    			}
				    		}
				    		if (zTMap[k]>d){
			    				rt[k]=d;
			    				zTMap[k]= d;
			    				tactileMap[k]=tactile;
			    				if (k==ai1) cornerT[k]=2;
			    				else        cornerT[k]=0;	
			    			}
				    	}
					}
				}
				
				// cells on the top left side
				if ( (j>0) && (i>0) && (Im_x-i>=0) && (Im_y+j<m_h) ){
					if (m_env.isWall(Im_x-i,Im_y+j) || m_env.isAlga(Im_x-i,Im_y+j) || m_env.isFood(Im_x-i,Im_y+j) ){
						Color bgc = m_env.m_blocks[Im_x-i][Im_y+j].seeBlock();
						int tactile=m_env.m_blocks[Im_x-i][Im_y+j].touchBlock();
						
						imin =(double)i-0.5 + (mPosition.x-Im_x);
						imin2=imin*imin;
						iplus=(double)i+0.5 + (mPosition.x-Im_x);
						jmin =(double)j-0.5 - (mPosition.y-Im_y);
						jmin2=jmin*jmin;
						jplus=(double)j+0.5 - (mPosition.y-Im_y);
						
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
				    		if (m_env.m_blocks[Im_x-i][Im_y+j].isVisible()){
				    			if (zVMap[k]>d){
				    				rv[k]=d;
				    				zVMap[k]= d;
				    				colorMap[k]=bgc;
				    				if      (k==ai2) cornerV[k]=1;
				    				else if (k==ai1) cornerV[k]=2;
				    				else             cornerV[k]=0;
				    			}
				    		}
				    		if (zTMap[k]>d){
			    				rt[k]=d;
			    				zTMap[k]= d;
			    				tactileMap[k]=tactile;
			    				if      (k==ai2) cornerT[k]=1;
			    				else if (k==ai1) cornerT[k]=2;
			    				else             cornerT[k]=0;
			    			}
				    	}		
				    	for (int k=ai1;k<=ai3;k++){
				    		d= d1*10 +   (d3-d1)*10*(k-ai1)/(ai3-ai1);
				    		if (m_env.m_blocks[Im_x-i][Im_y+j].isVisible()){
				    			if (zVMap[k]>d-0.01){
				    				rv[k]=d;
				    				zVMap[k]=d;
				    				colorMap[k]=bgc;
				    				if      (k==ai1) cornerV[k]=1;
				    				else if (k==ai3) cornerV[k]=2;
				    				else             cornerV[k]=0;
				    			}
				    		}
				    		if (zTMap[k]>d-0.01){
			    				rt[k]=d;
			    				zTMap[k]=d;
			    				tactileMap[k]=tactile;
			    				if      (k==ai1) cornerT[k]=1;
			    				else if (k==ai3) cornerT[k]=2;
			    				else             cornerT[k]=0;
			    			}
				    	}
						
					}
				}
				
				
				// agents detection
				for (int a=0;a<m_env.m_modelList.size();a++){
					Color bgc = m_env.AGENT;
					int tactile=m_env.CUDDLE;
					if (a!=ident){
						d= (mPosition.x-m_env.m_modelList.get(a).mPosition.x)*(mPosition.x-m_env.m_modelList.get(a).mPosition.x)
						  +(mPosition.y-m_env.m_modelList.get(a).mPosition.y)*(mPosition.y-m_env.m_modelList.get(a).mPosition.y);
						d=Math.sqrt(d);
						
						int ai1=0;
						int ai2=0;
						int ai3=0;
						int ai4=0;
						if (mPosition.x-m_env.m_modelList.get(a).mPosition.x<=0){
							a1=Math.toDegrees( Math.acos( (mPosition.y-m_env.m_modelList.get(a).mPosition.y)/d));
							ai1=180-(int)a1;
						}
						else{
							a1=Math.toDegrees( Math.acos( (mPosition.y-m_env.m_modelList.get(a).mPosition.y)/d));
							ai1=(int)a1+180;
						}
						
						a2=Math.atan(0.4/d);
						a2=Math.toDegrees(a2);
						
						ai2= (int)a2;
						
						ai3=ai1-ai2+360;
						ai4=ai1+ai2+360;
						
						int ai5=ai4-ai3;
						
						for (int k=ai3;k<ai4;k++){
							if (zVMap[k%360]>d*10){
								rv[k%360]=d*10 - 2*Math.sin(Math.PI*(k-ai3)/(ai4-ai3));
								zVMap[k%360]= d*10- 2*Math.sin(Math.PI*(k-ai3)/(ai4-ai3));
								colorMap[k%360]=bgc;
							}
							
							if (zTMap[k%360]>d*10){
								rt[k%360]=d*10 - 2*Math.sin(Math.PI*(k-ai3)/(ai4-ai3));
								zTMap[k%360]= d*10- 2*Math.sin(Math.PI*(k-ai3)/(ai4-ai3));
								tactileMap[k%360]=m_env.CUDDLE;
							}
						}
					}
				}
				
				
			}
		}
		
		
		
		for (int i=0;i<360;i++){
			int offset=(i-orientationDeg+630)%360;
			rv2[i]= rv[offset];
			colorMap2[i]=colorMap[offset];
			cornerV2[i]=cornerV[offset];
		}
		
		for (int i=0;i<360;i++){
			int offset=(i-orientationDeg+630)%360;
			rt2[i]= rt[offset];
			tactileMap2[i]=tactileMap[offset];
			cornerT2[i]=cornerT[offset];
		}
		
		for (int i=0;i<Ernest.RESOLUTION_RETINA;i++){
			retina[Ernest.RESOLUTION_RETINA-i-1]= new EyeFixation();
			retina[Ernest.RESOLUTION_RETINA-i-1].setColor(colorMap2[(int)(i*180/Ernest.RESOLUTION_RETINA+180/Ernest.RESOLUTION_RETINA/2+90)]);
			retina[Ernest.RESOLUTION_RETINA-i-1].setDistance((int) rv2[(int)(i*180/Ernest.RESOLUTION_RETINA+180/Ernest.RESOLUTION_RETINA/2+90)]);
		}
		
		if (sensor){
			colliculus.update(rv2, colorMap2, rt2, tactileMap2, lastAction, speed);
			//colliculusFrame.saveImage();
			//m_env.saveImage();
		}
		
		if (display){
			m_env.m_eye.repaint();
			m_env.m_eye.paint(rv2,colorMap2,cornerV2,rt2,tactileMap2,cornerT2);
		}
		
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
			int x0 =  Math.round(mPosition.x) + (int) ( 20 * Math.cos(angle) + .5); // round to the closest integer 
			int y0 =  Math.round(mPosition.y) - (int) ( 20 * Math.sin(angle) + .5); // Y axis is downwards
			
			eyeFixation = rayTrace(Math.round(mPosition.x), Math.round(mPosition.y) , x0, y0);
			
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
	    int x = Math.round(x0);
	    int y = Math.round(y0);
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

	    	Color bgc = m_env.m_blocks[x][y].seeBlock();
	    	if (m_env.isWall(x,y) || m_env.isAlga(x,y) || m_env.isFood(x,y))
	    	{
	    		if (m_env.isWall(x,y))
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

	
//	public int[][] somatoMap()
//	{
//		int[][] somatoMap = new int[3][3];
//		somatoMap[1][1] = soma(m_x, m_y);
//
//		// Orientation up [0, PI/8]
//		if (m_orientationAngle < ORIENTATION_UP * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[0][0] = soma(m_x - 1, m_y -1);
//			somatoMap[1][0] = soma(m_x , m_y - 1);
//			somatoMap[2][0] = soma(m_x + 1, m_y - 1);
//			somatoMap[2][1] = soma(m_x + 1, m_y);
//			somatoMap[2][2] = soma(m_x + 1, m_y + 1);
//			somatoMap[1][2] = soma(m_x, m_y + 1);
//			somatoMap[0][2] = soma(m_x - 1, m_y + 1);
//			somatoMap[0][1] = soma(m_x - 1, m_y);			
//		}
//		// Orientation up right [PI/8, 3PI/8]
//		else if (m_orientationAngle < ORIENTATION_UP_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[0][1] = soma(m_x - 1, m_y -1);
//			somatoMap[0][0] = soma(m_x , m_y - 1);
//			somatoMap[1][0] = soma(m_x + 1, m_y - 1);
//			somatoMap[2][0] = soma(m_x + 1, m_y);
//			somatoMap[2][1] = soma(m_x + 1, m_y + 1);
//			somatoMap[2][2] = soma(m_x, m_y + 1);
//			somatoMap[1][2] = soma(m_x - 1, m_y + 1);
//			somatoMap[0][2] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation right [3PI/8, 5PI/8]]
//		else if (m_orientationAngle < ORIENTATION_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[0][2] = soma(m_x - 1, m_y -1);
//			somatoMap[0][1] = soma(m_x , m_y - 1);
//			somatoMap[0][0] = soma(m_x + 1, m_y - 1);
//			somatoMap[1][0] = soma(m_x + 1, m_y);
//			somatoMap[2][0] = soma(m_x + 1, m_y + 1);
//			somatoMap[2][1] = soma(m_x, m_y + 1);
//			somatoMap[2][2] = soma(m_x - 1, m_y + 1);
//			somatoMap[1][2] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation down right [5PI/8, 7PI/8]
//		else if (m_orientationAngle < ORIENTATION_DOWN_RIGHT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[1][2] = soma(m_x - 1, m_y -1);
//			somatoMap[0][2] = soma(m_x , m_y - 1);
//			somatoMap[0][1] = soma(m_x + 1, m_y - 1);
//			somatoMap[0][0] = soma(m_x + 1, m_y);
//			somatoMap[1][0] = soma(m_x + 1, m_y + 1);
//			somatoMap[2][0] = soma(m_x, m_y + 1);
//			somatoMap[2][1] = soma(m_x - 1, m_y + 1);
//			somatoMap[2][2] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation down [7PI/8, 9PI/8]
//		else if (m_orientationAngle < ORIENTATION_DOWN * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[2][2] = soma(m_x - 1, m_y -1);
//			somatoMap[1][2] = soma(m_x , m_y - 1);
//			somatoMap[0][2] = soma(m_x + 1, m_y - 1);
//			somatoMap[0][1] = soma(m_x + 1, m_y);
//			somatoMap[0][0] = soma(m_x + 1, m_y + 1);
//			somatoMap[1][0] = soma(m_x, m_y + 1);
//			somatoMap[2][0] = soma(m_x - 1, m_y + 1);
//			somatoMap[2][1] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation down left [9PI/8, 11PI/8]
//		else if (m_orientationAngle < ORIENTATION_DOWN_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[2][1] = soma(m_x - 1, m_y -1);
//			somatoMap[2][2] = soma(m_x , m_y - 1);
//			somatoMap[1][2] = soma(m_x + 1, m_y - 1);
//			somatoMap[0][2] = soma(m_x + 1, m_y);
//			somatoMap[0][1] = soma(m_x + 1, m_y + 1);
//			somatoMap[0][0] = soma(m_x, m_y + 1);
//			somatoMap[1][0] = soma(m_x - 1, m_y + 1);
//			somatoMap[2][0] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation left [11PI/8, 13PI/8]
//		else if (m_orientationAngle < ORIENTATION_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[2][0] = soma(m_x - 1, m_y -1);
//			somatoMap[2][1] = soma(m_x , m_y - 1);
//			somatoMap[2][2] = soma(m_x + 1, m_y - 1);
//			somatoMap[1][2] = soma(m_x + 1, m_y);
//			somatoMap[0][2] = soma(m_x + 1, m_y + 1);
//			somatoMap[0][1] = soma(m_x, m_y + 1);
//			somatoMap[0][0] = soma(m_x - 1, m_y + 1);
//			somatoMap[1][0] = soma(m_x - 1, m_y);			
//		}
//		
//		// Orientation up left [13PI/8, 15PI/8]
//		else if (m_orientationAngle < ORIENTATION_UP_LEFT * Math.PI/2 / ORIENTATION_RIGHT + Math.PI/8)
//		{
//			somatoMap[1][0] = soma(m_x - 1, m_y -1);
//			somatoMap[2][0] = soma(m_x , m_y - 1);
//			somatoMap[2][1] = soma(m_x + 1, m_y - 1);
//			somatoMap[2][2] = soma(m_x + 1, m_y);
//			somatoMap[1][2] = soma(m_x + 1, m_y + 1);
//			somatoMap[0][2] = soma(m_x, m_y + 1);
//			somatoMap[0][1] = soma(m_x - 1, m_y + 1);
//			somatoMap[0][0] = soma(m_x - 1, m_y);			
//		}
//		// Orientation up [15PI/8, 2PI]
//		else
//		{
//			somatoMap[0][0] = soma(m_x - 1, m_y -1);
//			somatoMap[1][0] = soma(m_x , m_y - 1);
//			somatoMap[2][0] = soma(m_x + 1, m_y - 1);
//			somatoMap[2][1] = soma(m_x + 1, m_y);
//			somatoMap[2][2] = soma(m_x + 1, m_y + 1);
//			somatoMap[1][2] = soma(m_x, m_y + 1);
//			somatoMap[0][2] = soma(m_x - 1, m_y + 1);
//			somatoMap[0][1] = soma(m_x - 1, m_y);			
//		}
//		
//		return somatoMap;
//	}
//	
//	public int soma(float m_x, float m_y)
//	{
//		int soma = 0;
//		if (m_x < 0 || m_y < 0 || m_x >= m_w || m_y >= m_h)
//			return Ernest.STIMULATION_TOUCH_WALL.getValue();
//		if (isDirty(Math.round(m_x),Math.round(m_y)))
//		{
//			if (getDirty(Math.round(m_x),Math.round(m_y)) == DIRTY)
//				soma = Ernest.STIMULATION_TOUCH_FISH.getValue();
//			else
//				soma = Ernest.STIMULATION_TOUCH_SOFT.getValue();
//		}
//		if (isWall(Math.round(m_x),Math.round(m_y))) soma = Ernest.STIMULATION_TOUCH_WALL.getValue();
//		return soma;
//	}
	
	/**
	 * Paint Ernest as a shark.
	 * @param g The graphic object for painting.
	 */
	public void paintAgent(Graphics2D g2d,int x,int y,double sx,double sy)
	{
		
		// The orientation

		AffineTransform orientation = new AffineTransform();
		orientation.translate(x,y);
		orientation.rotate(-mOrientation.z+Math.PI/2);
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
		g2d.setColor(m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock() );
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
//				//eyeColor = new Color(m_ernest.getObservation().getSalience().getColor().getRGB());
//				eyeColor = new Color(m_ernest.getObservation().getSalience().getBundle().getVisualStimulation().getValue());
//			}
						
			// Somatomap color
			for (int i = 0; i < 3; i++)
				for (int j = 0; j < 3; j++)
					somatoMapColor[i][j] = new Color(m_ernest.getValue(i, j));
			
//			if (m_ernest.getObservation().getLabel() == null)
//				System.out.println("Observation has no label");
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
			/*for (int act=0;act<m_actionList.size();act++){
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
							//file.print(m_actionList.get(act).confidenceMap.get(o)[i][j]+" ");
						}
						file.println();
					}
				}
			}*/
			/*
			// object list
			for (int i=0;i<m_objMemory.objectList.size();i++){
				file.println("object "+m_objMemory.objectList.get(i).getRed()  +" "+
									   m_objMemory.objectList.get(i).getGreen()+" "+
									   m_objMemory.objectList.get(i).getBlue() +" "+
									   m_objMemory.value.get(i));
			}*/

			
			// primitive Schemas
			/*int nb=m_ernest.get(0).getEpisodicMemory().m_schemas.size();
			for (int i=0;i<nb;i++){
				if (m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).isPrimitive()){
					file.println("schema "+1+" "+m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getId()+" "+
											     m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getLabel()+" "+
											     m_ernest.get(0).getEpisodicMemory().m_schemas.get(i).getWeight());
				}
			}*/
			
			
			
			// sensors positions and connections
			file.println("sensors "+m_tactile.resolution+" "+m_tactile.sensorRes);
			file.print("sensorX");
			for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
				file.print(" "+m_tactile.sensorX[i]);
			}
			file.println();
			
			file.print("sensorY");
			for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
				file.print(" "+m_tactile.sensorY[i]);
			}
			file.println();
			
			// sensorConnections
			file.println("sensorConnections");
			for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
				for (int j=0;j<m_tactile.resolution*m_tactile.sensorRes;j++){
					file.print(m_tactile.connections[i][j]+" ");
				}
				file.println();
			}
			
			// sensor confidence
			file.println("sensorConfidence");
			for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
				for (int j=0;j<m_tactile.resolution*m_tactile.sensorRes;j++){
					file.print(m_tactile.connectConfidence[i][j]+" ");
				}
				file.println();
			}
			
			file.close();
			
			System.out.println("file saved");

		} catch (Exception e) {
			System.out.print("Erreur : ");
			e.printStackTrace();
		}


	}

	// load a ernest file
	//public boolean load(ArrayList<Action> actList){
	public boolean load(){
		String fileName = "/home/simon/Bureau/Ernest.txt";
		
		boolean succes=true;
		int nbLine=0;
		int nbAct=0;
		int nbObj=0;
		int indexObj=0;
		int matrixType=0;      // 1->selectMap, 2->confidenceMap , 3->sensor connections, 4->sensor confidence
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
			    			/*if (elements.length == 4){
			    				actList.add(new Action(elements[1],10,Integer.parseInt(elements[2])*5,Integer.parseInt(elements[3])*5,m_objMemory));
			    				nbAct++;
			    				nbObj=0;
			    				indexObj=0;
			    				indexLine=0;
			    				h=Integer.parseInt(elements[3]);
			    			}
			    			else succes=false;*/
			    		}
			    		
			    		// case selectMap
			    		else if (elements[0].equals("selectMap")){
			    			/*if (elements.length == 2 && nbAct>0){
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
			    			else succes=false;*/
			    		}
			    		
			    		/*
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
			    		}*/
			    		
			    		
			    		// case object
			    		else if (elements[0].equals("object")){
			    			/*if (elements.length == 5){
			    				
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
			    			}*/
			    			
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
			    		
			    		
			    		// sensor map
			    		else if (elements[0].equals("sensors")){
			    			if (elements.length == 3){
			    				m_tactile=new TactileMap(this, Integer.parseInt(elements[1]),Integer.parseInt(elements[2]));
			    			}
			    		}
			    		
			    		// sensorX vector
			    		else if (elements[0].equals("sensorX")){
			    			if (elements.length == m_tactile.resolution*m_tactile.sensorRes+1){
			    				for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
			    					m_tactile.sensorX[i]= Double.parseDouble(elements[i+1]);
			    				}
			    			}
			    		}
			    		// sensorY vector
			    		else if (elements[0].equals("sensorY")){
			    			if (elements.length == m_tactile.resolution*m_tactile.sensorRes+1){
			    				for (int i=0;i<m_tactile.resolution*m_tactile.sensorRes;i++){
			    					m_tactile.sensorY[i]= Double.parseDouble(elements[i+1]);
			    				}
			    			}
			    		}
			    		
			    		
			    		
			    		// sensor connections
			    		else if (elements[0].equals("sensorConnections")){
			    			matrixType=3;
		    				indexLine=0;
			    		}
			    		
			    		// sensor confidence
			    		else if (elements[0].equals("sensorConfidence")){
			    			matrixType=4;
		    				indexLine=0;
			    		}
			    		
			    		
			    		// matrix value (no label)
			    		else{
			    			/*if (indexLine<actList.get(nbAct-1).width){
			    			int min=Math.min(h, elements.length);
			    			for (int j=0;j<min;j++){
			    				if (matrixType==1) actList.get(nbAct-1).selectMap.get(indexObj)[indexLine][j]=Float.parseFloat(elements[j]);
			    				if (matrixType==2) actList.get(nbAct-1).confidenceMap.get(indexObj)[indexLine][j]=Float.parseFloat(elements[j]); 
			    			}
			    			if (matrixType==3 || matrixType==4){
			    				for (int j=0;j<m_tactile.resolution*m_tactile.sensorRes;j++){
			    					if (matrixType==3) m_tactile.connections[indexLine][j]= Double.parseDouble(elements[j]);
			    					if (matrixType==4) m_tactile.connectConfidence[indexLine][j] = Double.parseDouble(elements[j]);
			    				}
			    			}
			   				indexLine++;
			    			};*/
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
		/*
		for (int k=0;k<actList.size();k++){
			for (int i=0;i<m_objMemory.objectList.size();i++){
				for (int j=0;j<m_objMemory.objectList.size();j++){
					
					if (i==j) actList.get(k).distances.get(i).set(j, (float) 0);
					else      actList.get(k).distances.get(i).set(j, actList.get(k).distance(m_objMemory,
																							 m_objMemory.objectList.get(i),
																							 m_objMemory.objectList.get(j)));
					
				}
			}
		}*/
		
		return succes;
	}

}