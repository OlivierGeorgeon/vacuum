package agent;

import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;

import memory.ColliculusFrame;
import memory.TactileMapFrame;
import memory.VisualMapFrame;
import memory110.SpaceMemory;
import memory110.SpaceMemoryFrame;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import ernest.*;
import spas.IAffordance;
import spas.IPlace;
import spas.LocalSpaceMemory;
import spas.Spas;
import tracing.*;
import utils.ErnestUtils;
import utils.Pair;

/**************************************
 * A Model for a fish that moves in the environment
 * @author ogeorgeon
 **************************************/
public class Ernest120Model extends ErnestModel 
{
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
    final static float ROTATION_FRICTION = .9f; // .95f
    
    final static float SLIPPERY_EFFECT = 4.8f;

    private static Color UNANIMATED_COLOR = Color.GRAY;
    
    public SpaceMemory m_SpaceMemory;
    
    Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA];
    Color[][] somatoMapColor = new Color[3][3];
    Color focusColor = UNANIMATED_COLOR;
    Color leftColor = UNANIMATED_COLOR;
    Color rightColor = UNANIMATED_COLOR;
    
    boolean m_status = true;
    
    /**
     * @param i The agent's numerical id. 
     */
    public Ernest120Model(int i) 
    {
        super(i);
    }
    
    /**
     * Initialize the agent in the grid
     */
    public void init(int w,int h) throws Exception
    {
        // Initialize the model
        super.init(w,h);
        
        // Initialize Ernest's colors 
        for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
            pixelColor[i] = UNANIMATED_COLOR;
        
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
                somatoMapColor[i][j] = UNANIMATED_COLOR;
        
        focusColor = UNANIMATED_COLOR;
        leftColor =  UNANIMATED_COLOR;
        rightColor =  UNANIMATED_COLOR;

        setChanged();
        notifyObservers2();  
        
        m_SpaceMemory=new SpaceMemory();
    }

    /**
     * @return The version of the Ernest model
     */
    public String getVersion()
    {
        return "Ernest 12.0";
    }
    
    /**
     * Initialize the Ernest agent.
     */
    public void initErnest()
    {
    	// Instantiate Ernest
        m_ernest = new Ernest();
        //m_sensorymotorSystem = new SpatialSensorimotorSystem();
        
        // Initialize the visualization.
		m_SpaceMemory.setModel(this);
		m_eye.setModel(this);
        
        // Only trace the first agent.
        
//        if (ident == 8)
//       	m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","h-yXVWrEwtYclxuPQUlmTOXprcFzol");
                        
        // Initialize the Ernest === 
        
        // Ernest's inborn primitive interactions

        m_ernest.setParameters(6, 10);
        m_ernest.setTracer(m_tracer);
        m_ernest.setSensorymotorSystem(new Ernest12SensorimotorSystem());

        m_ernest.addInteraction("-", "f",  -10); // Touch empty
        m_ernest.addInteraction("-", "t",  -20); // Touch wall
        m_ernest.addInteraction("\\","f",  -10); // Touch right empty
        m_ernest.addInteraction("\\","t",  -20); // Touch right wall
        m_ernest.addInteraction("/", "f",  -10); // Touch left empty
        m_ernest.addInteraction("/", "t",  -20); // Touch left wall
        m_ernest.addInteraction(">", "t",   50); // Move
        m_ernest.addInteraction(">", "f",  -100); // Move
        m_ernest.addInteraction("v", "t",  -30); // Right toward empty
        m_ernest.addInteraction("v", "f",  -30); // Right toward empty
        m_ernest.addInteraction("^", "t",  -30); // Left toward empty
        m_ernest.addInteraction("^", "f",  -30); // Left toward empty

		cognitiveMode = AGENT_RUN;
        mTranslation = new Vector3f();
        mRotation  = new Vector3f();
        
        System.out.println("Ernest initialized") ;
    }
    
    /**
     * Initialize the agent's parameters
     */
    protected void initAgent()
    {
    }
    
	public void setDisplay(){
		
		boolean dispSpaceMemory=true;
		boolean dispEyeView=true;
		boolean dispInnerEar=false;
		
		int size;
		int i;
		boolean found;
		
		//////////////////////
		if (dispSpaceMemory){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				System.out.println(m_env.frameList.get(i).getClass().getName());
				if (m_env.frameList.get(i).getClass().getName().equals("memory110.SpaceMemoryFrame")) found=true;
				i++;
			}
			if (!found) m_env.frameList.add(new SpaceMemoryFrame(m_SpaceMemory));
			else        
				((SpaceMemoryFrame)m_env.frameList.get(i-1)).setMemory(m_SpaceMemory);
		}
		
		///////////////////
		if (dispEyeView){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				if (m_env.frameList.get(i).getClass().getName().equals("agent.EyeView")) found=true;
				i++;
			}
		
			if (!found) m_env.frameList.add(new EyeView(m_eye)); 
			else        ((EyeView) m_env.frameList.get(i-1)).setEye(m_eye);
		}
		
		///////////////////
		if (dispInnerEar){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				if (m_env.frameList.get(i).getClass().getName().equals("InnerEar")) found=true;
				i++;
			}	
			if (!found) m_env.frameList.add(new InnerEarFrame(m_ear)); 
			else        ((InnerEarFrame) m_env.frameList.get(i-1)).setInnerEar(m_ear);
		}
	}
    
    
    
    /**
     * Update the agent when the environment is refreshed.
     * (not necessarily a cognitive step for the agent).
     */
    public void update()
    {
    	//count();

		String schema = m_ernest.step(m_status);
		int[] intention = new int[2]; 
		intention[0]= schema.toCharArray()[0];

		
        if (cognitiveMode == AGENT_STEP)
        	cognitiveMode = AGENT_STOP;

		enactSchema(intention);
        anim();
    }
    
    /**
     * Enact the primitive schema chosen by Ernest.
     */
    private void enactSchema(int[] schema)
    {
        focusColor = UNANIMATED_COLOR;
        leftColor =  UNANIMATED_COLOR;
        rightColor =  UNANIMATED_COLOR;

        if (schema[0] != 0)
    	{
    		
	        m_schema = Character.toString((char)schema[0]);
	        int impulsion = schema[1];
	
	        // A new interaction cycle is starting
	        System.out.println("Agent #"+ident+", Step #" + getCounter() + "=======");
	        
	        if (schema[0] == 'v')
	        {
	        	mOrientation.z -= Math.PI/2;
	        	if (mOrientation.z < - Math.PI) mOrientation.z += 2 * Math.PI; 
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
	        	m_status = false;
	            //m_status = m_env.affordWalk(point);
	        }
	        else if (schema[0] == '^')
	        {
	        	mOrientation.z += Math.PI/2;
	        	if (mOrientation.z > Math.PI) mOrientation.z -= 2 * Math.PI; 
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
	        	m_status = false;
	            //m_status = m_env.affordWalk(point);
	        }
	        else if (schema[0] == '>')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
	            m_status = m_env.affordWalk(point);
	            if (m_status)
	            	mPosition.set(localToParentRef(new Vector3f(DIRECTION_AHEAD)));
	            else
	            	focusColor = Color.RED;
	        }
	        else if (schema[0] == '-')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
	            m_status = !m_env.affordWalk(point);
	            if (m_status)
	            	focusColor = Environment.WALL1;
	            else
	            	focusColor = Color.WHITE;
	        }
	        else if (schema[0] == '/')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_LEFT);
	        	Vector3f point = localToParentRef(localPoint);
	            m_status = !m_env.affordWalk(point);
	            if (m_status)
	            	leftColor = Environment.WALL1;
	            else
	            	leftColor = Color.WHITE;
	        }
	        else if (schema[0] == '\\')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_RIGHT);
	        	Vector3f point = localToParentRef(localPoint);
	            m_status = !m_env.affordWalk(point);
	            if (m_status)
	            	rightColor = Environment.WALL1;
	            else
	            	rightColor = Color.WHITE;
	        }
	        
	        // Trace the environmental data
	        if (m_tracer != null)
	        {
				//Object environment = m_tracer.newEvent("environment", "position", m_counter);
				m_tracer.addEventElement("x", mPosition.x + "");
				m_tracer.addEventElement("y", mPosition.y + "");
				m_tracer.addEventElement("orientation", mOrientation.z + "");
	        }               
    	}
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
        orientation.rotate(-mOrientation.z+Math.PI/2);
        orientation.scale(sx,sy);
        g2d.transform(orientation);

        // The shark body

        Area shark = shape(ident);
        
        // Retina pixel
        
        //Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
        Arc2D.Double focus = new Arc2D.Double(-10, -35, 20, 20,0, 180, Arc2D.PIE);
        Arc2D.Double left = new Arc2D.Double(-10, -35, 20, 20,90, 180, Arc2D.PIE);
        Arc2D.Double right = new Arc2D.Double(-10, -35, 20, 20,-90, 180, Arc2D.PIE);
        
        // The tactile matrix
        
        Area[][] somatoMap = new Area[3][3];
        somatoMap[0][0] = new Area(new Rectangle2D.Double(-49, -47, 42, 40));
        somatoMap[0][0].intersect(shark);
        somatoMap[1][0] = new Area(new Rectangle2D.Double( -8, -47, 16, 40));
        somatoMap[1][0].intersect(shark);
        somatoMap[2][0] = new Area(new Rectangle2D.Double(  7, -47, 42, 40));
        somatoMap[2][0].intersect(shark);
        
        somatoMap[0][1] = new Area(new Rectangle2D.Double(-49,  -8, 42, 16));
        somatoMap[0][1].intersect(shark);
        somatoMap[1][1] = new Area(new Rectangle2D.Double( -8,  -8, 16, 16));
        somatoMap[1][1].intersect(shark);
        somatoMap[2][1] = new Area(new Rectangle2D.Double(  7,  -8, 42, 16));
        somatoMap[2][1].intersect(shark);

        somatoMap[0][2] = new Area(new Rectangle2D.Double(-49,   6, 42, 42));
        somatoMap[0][2].intersect(shark);
        somatoMap[1][2] = new Area(new Rectangle2D.Double( -8,   6, 16, 42));
        somatoMap[1][2].intersect(shark);
        somatoMap[2][2] = new Area(new Rectangle2D.Double(  7,   6, 42, 42));
        somatoMap[2][2].intersect(shark);

        // Draw the body
        
        g2d.setColor(ErnestModel.AGENT_COLOR);
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
            {
                //g2d.setColor(somatoMapColor[i][j]);
                g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_SOFT));
            	if (m_ernest.getValue(i, j) == Ernest.PHENOMENON_EMPTY)
            		g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_EMPTY));
            	if (m_ernest.getValue(i, j) == Ernest.PHENOMENON_WALL)
            		g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_WALL));
                g2d.fill(somatoMap[i][j]);
            }
        
        // Draw the focus
        
    	g2d.setColor(leftColor);
        if (leftColor != UNANIMATED_COLOR)
        	g2d.fill(left);
        g2d.setColor(rightColor);
        if (rightColor != UNANIMATED_COLOR)
        g2d.fill(right);
        if (focusColor != UNANIMATED_COLOR)
        {
        	g2d.setColor(focusColor);
        	g2d.fill(focus);
        }
    }

    /**
     * The shape is centered in (0,0) and fits in a 100x100 rectangle.
     * The pelvic fin pattern represents the agent's id number in binary code.
     * @param ID The agent's id number.
     * @return The shark area.
     */
    public static Area shape(int ID)
    {
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
    	shark.add(new Area(leftPectoralFin));
        if ((ID & 1) == 0)
        	shark.add(new Area(leftPelvicFin));
        shark.add(new Area(rightPectoralFin));
        if ((ID & 2) == 0)
        	shark.add(new Area(rightPelvicFin));
        
        return shark;
    }    
    
    private void anim()
    {
        // compute absolute movements
		mSpeedT=new Vector3f(mPosition);
		mSpeedT.sub(mPreviousPosition);
		
		mSpeedR=new Vector3f(mOrientation);
		mSpeedR.sub(mPreviousOrientation);
		
		Matrix3f rot2 = new Matrix3f();
		rot2.rotZ( -mOrientation.z);
		rot2.transform(mSpeedT, mEgoSpeedT);
		
		//m_ear.computeEars(mEgoSpeedT, mSpeedR);
		
		if (mSpeedR.z > Math.PI) mSpeedR.z-=2*Math.PI;
		if (mSpeedR.z<=-Math.PI) mSpeedR.z+=2*Math.PI;
		
        if (cognitiveMode!=AGENT_STOP) rendu();
        
        mainFrame.drawGrid();
        
        for (int i=0;i<m_env.frameList.size();i++){
			m_env.frameList.get(i).repaint();
		}
    }
    
	public void paintSpaceMemory(Graphics g)
	{
		final int WIDTH = 300;
		final int HEIGHT = 250;
		final int SCALE = 35;//40; 

		boolean displayPhenomenon = true;
		
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

		// Display background
		g2d.setColor(Color.white);
		//g2d.fillRect(0, 0, 2 * RADIUS * SCALE, 2 * RADIUS * SCALE);
		
        // Display counter
		String counter = getCounter() + ""; 
		Font font = new Font("Dialog", Font.BOLD, 18);
		g2d.setFont(font);
		//FontMetrics fm = getFontMetrics(font);
		//int width = fm.stringWidth(counter);
		g2d.setColor(Color.GRAY);		
		//g2d.drawString(counter, 2 * WIDTH - 30 - width, 30);	
		g2d.drawString(counter, 2 * WIDTH - 50, 30);	
		
		float agentOrientation = 0;
//		for (IPlace place : spaceMemory.getPlaceList())
//		{
//			if (place.getType() == Spas.PLACE_FOCUS)
//			{
//				refAngle = place.getDirection();
//			}
//		}

		IPlace focusPlace = getErnest().getFocusPlace();
		agentOrientation = getOrientation();
		
		//float baseOrientation = - agentOrientation + focusPlace.getOrientation();
		float baseOrientation = agentOrientation; // allocentric
		//float baseOrientation = - focusPlace.getOrientation(); // target centric.
		//float baseOrientation = 0; // agent horizontal
		//float x = (float)Math.cos(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		//float y = (float)Math.sin(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		
		AffineTransform ref0 = g2d.getTransform();
		AffineTransform ref1 = new AffineTransform();
		//ref1.translate( - x * SCALE,  y * SCALE);
        //ref1.rotate(- baseOrientation, WIDTH, HEIGHT);
        g2d.transform(ref1);
        //g2d.setTransform(ref0);
		
		// Reference transformation
        AffineTransform ref = g2d.getTransform();
        
        //g2d.setTransform(ref);
        
		double d;
		double rad;
		double angle;
		double span;
		
        // Display the places
		//g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
		g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER));

		Ellipse2D.Double circle = new Ellipse2D.Double(-10, -10, 20, 20); 
        Arc2D.Double pie = new Arc2D.Double(-10, -10, 20, 20,-90, 180, Arc2D.PIE);
        Polygon triangle = new Polygon();triangle.addPoint(-10,-10);triangle.addPoint(-10,10);triangle.addPoint(10,0);
        int squareSize = 7;
        Polygon square = new Polygon();square.addPoint(-squareSize,-squareSize);square.addPoint(-squareSize,squareSize);square.addPoint(squareSize,squareSize);square.addPoint(squareSize,-squareSize);
        Polygon tile = new Polygon();tile.addPoint(-SCALE/2,-SCALE/2);tile.addPoint(-SCALE/2,SCALE/2);tile.addPoint(SCALE/2,SCALE/2);tile.addPoint(SCALE/2,-SCALE/2);

 		// Display the phenomenon
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		AffineTransform or;
		for (IPlace place : getErnest().getPlaceList())
		{
			if (place.getType() == Spas.PLACE_PHENOMENON)
			{
				g2d.setColor(new Color(place.getValue()));		
				
		        ref = g2d.getTransform();
		        or = new AffineTransform();
		        or.translate(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE));
		        or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
		        or.rotate(- place.getOrientation());
		        g2d.transform(or);
				g2d.fill(tile);
				g2d.setColor(Color.black);
				g2d.draw(tile);
		        g2d.setTransform(ref);
			}
		}
				
 		// Display the interactions
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		//AffineTransform or;
		for (IPlace place : getErnest().getPlaceList())
		{
			if (place.getType() == Spas.PLACE_EVOKE_PHENOMENON)
			{
				g2d.setColor(new Color(place.getValue()));		
				
				Shape shape = circle;
				if (place.getShape() == Spas.SHAPE_TRIANGLE)
					shape = triangle;
				else if (place.getShape() == Spas.SHAPE_PIE)
					shape = pie;
				else if (place.getShape() == Spas.SHAPE_SQUARE)
					shape = square;
				
		        ref = g2d.getTransform();
		        or = new AffineTransform();
		        or.translate(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE));
		        or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
		        or.rotate(- place.getOrientation());
		        g2d.transform(or);
				g2d.fill(shape);
				g2d.setColor(Color.black);
				g2d.draw(shape);
		        g2d.setTransform(ref);
				//g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
	
				//g2d.drawLine(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE), 
				//	WIDTH + (int)(place.getSecondPosition().x * SCALE), HEIGHT - (int)(place.getSecondPosition().y * SCALE));
			}
		}

		// Display agent
        g2d.setTransform(ref);
        AffineTransform placeAgent = new AffineTransform();
        placeAgent.translate(WIDTH, HEIGHT);
        placeAgent.rotate(Math.PI/2);
        placeAgent.scale(SCALE / 100f, SCALE / 100f);
        g2d.transform(placeAgent);
		g2d.setColor(Color.gray);
        g2d.fill(shape(getID()));

	}

}
