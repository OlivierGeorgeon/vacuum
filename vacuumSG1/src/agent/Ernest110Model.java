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
import java.awt.FontMetrics;
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
public class Ernest110Model extends ErnestModel 
{
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
    final static float ROTATION_FRICTION = .9f; // .95f
    
    final static float SLIPPERY_EFFECT = 4.8f;

    private static Color UNANIMATED_COLOR = Color.GRAY;
    
    public SpaceMemory m_SpaceMemory;
    public List<IPlace> placeList;
    
    Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA];
    Color[][] somatoMapColor = new Color[3][3];
    Color focusColor = UNANIMATED_COLOR;
    
    /**
     * @param i The agent's numerical id. 
     */
    public Ernest110Model(int i) 
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

        setChanged();
        notifyObservers2();  
        
        m_SpaceMemory=new SpaceMemory();
        placeList=new ArrayList<IPlace>();
    }

    /**
     * @return The version of the Ernest model
     */
    public String getVersion()
    {
        return "Ernest 11.0";
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
        
        m_ernest.setParameters(5, 4);
        m_ernest.setTracer(m_tracer);
        m_ernest.setSensorymotorSystem(m_sensorymotorSystem);

        // Ernest's inborn primitive interactions
        
        m_ernest.addInteraction(">", " ",   20); // Move
        m_ernest.addInteraction("^", " ",  -10); // Left toward empty
        m_ernest.addInteraction("v", " ",  -10); // Right toward empty
        
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
    	// Sense the environment on each update.
		int[][] sense = sense();
    	
        sense[2][8] = (int)(mEgoSpeedT.x * Ernest.INT_FACTOR);
        sense[3][8] = (int)(mEgoSpeedT.y * Ernest.INT_FACTOR);
        sense[4][8] = (int)(mSpeedR.z * Ernest.INT_FACTOR); 
        sense[5][8] = (int)(mTranslation.length() * Ernest.INT_FACTOR);
        sense[6][8] = cognitiveMode;
        
        // Update Ernest.

		m_ernest.setSegmentList(m_eye.segments);
        
		int[] intention = m_ernest.update(sense);
		// if (cognitiveMode!=AGENT_STOP) rendu();
		
        if (intention[0] != 0 && cognitiveMode == AGENT_STEP)
        	cognitiveMode = AGENT_STOP;

		enactSchema(intention);
		
		// Refresh the local space memory window
		//m_SpaceMemory.update( (ArrayList<IPlace>) getPlaceList() );
		//m_SpaceMemory.update( (ArrayList<IPlace>) m_ernest.getPlaceList());
		//m_SpaceMemory.update();

		
		//if (cognitiveMode!=AGENT_STOP) rendu();
        anim();
    }
    
    /**
     * Sense the environment.
     */
    private int[][] sense()
    {
        // See the environment ===

        // 12 visual pixels * 8 visual info + 1 miscelaneous + 1 tactile
        int [][] matrix = new int[Ernest.RESOLUTION_RETINA][8 + 1 + 1];
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
            matrix[i][5] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getRed();
            matrix[i][6] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getGreen();
            matrix[i][7] = m_env.m_blocks[Math.round(mPosition.x)][Math.round(mPosition.y)].seeBlock().getBlue();
            
            // For display
            pixelColor[i] = eyeFixation[i].mRight;
        }
        
        // Taste ===
        
        matrix[0][8] = Ernest.STIMULATION_GUSTATORY_NOTHING;
        if (m_eat)
            matrix[0][8] = Ernest.STIMULATION_GUSTATORY_FISH;
        if (m_cuddle)
            matrix[0][8] = Ernest.STIMULATION_GUSTATORY_CUDDLE;
        
        // Kinematic (simulates sensors of body action) ===
        
        if (m_schema.equals(">"))
               matrix[1][8] = (!m_bump ? Ernest.STIMULATION_KINEMATIC_FORWARD : Ernest.STIMULATION_KINEMATIC_BUMP);
        else
        	matrix[1][8] = Ernest.STIMULATION_KINEMATIC_FORWARD;
        
        // Tactile ===
        
        int [] somatoMap = somatoMap();
        
//        if (m_cuddle)
//        	// If Ernest is cuddling then he is also touching ahead.
//        	somatoMap[3] = Ernest.STIMULATION_TOUCH_AGENT;
//        if (m_eat)
//        	// If Ernest is eating then he is also touching ahead.
//        	somatoMap[3] = Ernest.STIMULATION_TOUCH_FISH;

        for (int i = 0; i < 9; i++)
        	matrix[i][9] = somatoMap[i];
        
        // For display
        somatoMapColor[2][2] = new Color(somatoMap[0]);
        somatoMapColor[2][1] = new Color(somatoMap[1]);
        somatoMapColor[2][0] = new Color(somatoMap[2]);
        somatoMapColor[1][0] = new Color(somatoMap[3]);
        somatoMapColor[0][0] = new Color(somatoMap[4]);
        somatoMapColor[0][1] = new Color(somatoMap[5]);
        somatoMapColor[0][2] = new Color(somatoMap[6]);
        somatoMapColor[1][2] = new Color(somatoMap[7]);
        somatoMapColor[1][1] = new Color(somatoMap[8]);

        return matrix;
    }
    
    /**
     * Enact the primitive schema chosen by Ernest.
     */
    private void enactSchema(int[] schema)
    {
    	if (schema[0] != 0)
    	{
    		
    		m_bump = false;
    		m_eat = false;
    		m_cuddle = false;
    		
	        //m_schema = schema;
	        m_schema = Character.toString((char)schema[0]);
	        int impulsion = schema[1];
	
	        // A new interaction cycle is starting
	        System.out.println("Agent #"+ident+", Step #" + getCounter() + "=======");
	        
	        if (schema[0] == 'v')
	        	mRotation.add(new Vector3f(0, 0, (float) - impulsion / Ernest.INT_FACTOR));
	        else if (schema[0] == '^')
	        	mRotation.add(new Vector3f(0, 0, (float) impulsion / Ernest.INT_FACTOR));
	        else if (schema[0] == '>')
	            mTranslation.add(new Vector3f((float) impulsion / Ernest.INT_FACTOR, 0, 0));
	
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
     * Animate the agent in the environment 
     */
    private void anim()
    {
        boolean status = true;
        float HBradius = BOUNDING_RADIUS;  // radius of Ernest hitbox 
        
		mPreviousPosition.set(mPosition);
		mPreviousOrientation.set(mOrientation);
        
        // Move the agent
        rotate(mTranslation, - mRotation.z / SLIPPERY_EFFECT); // Creates some slippery in the turns.
        mPosition.set(localToParentRef(mTranslation));
        mOrientation.z += mRotation.z;
        
        if (mOrientation.z < - Math.PI) mOrientation.z += 2 * Math.PI;
        if (mOrientation.z > Math.PI)   mOrientation.z -= 2 * Math.PI;
        
        // Apply friction to the speed vectors
        mTranslation.scale(TRANSLATION_FRICTION);
        mRotation.scale(ROTATION_FRICTION);
        
        // Cuddle the agent ahead
        Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
        localPoint.scale(HBradius);
        Vector3f point = localToParentRef(localPoint);
        if (affordCuddle(point))
        {
            keepDistance(mPosition, entityCenter(point), 2 * HBradius );
            if (!m_cuddle)
                mTranslation.scale(.5f); // slowing down makes it look more like cuddling.
            m_cuddle = true;
        }        
        
        // Bumping ====

        // Stay away from north wall
        point = new Vector3f(DIRECTION_NORTH);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
            if (mOrientation.z > (float)Math.PI/4 && mOrientation.z < 3*(float)Math.PI/4)
                // It counts as a bump only if the angle is closer to perpendicular plus or minus PI/4
                status = false;
            mPosition.y = Math.round(point.y) - 0.5f - HBradius;
        }
        // Stay away from east wall
        point = new Vector3f(DIRECTION_EAST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
            if (mOrientation.z > - (float)Math.PI/4 && mOrientation.z < (float)Math.PI/4) 
                status = false;
            mPosition.x = Math.round(point.x) - 0.5f - HBradius;
        }
        // Stay away from south wall
        point = new Vector3f(DIRECTION_SOUTH);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
            if (mOrientation.z < - (float)Math.PI/4 && mOrientation.z > - 3 *(float)Math.PI/4)
                status = false;
            mPosition.y = Math.round(point.y) + 0.5f + HBradius;
        }
        // Stay away from west wall
        point = new Vector3f(DIRECTION_WEST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
            if (mOrientation.z > 3*(float)Math.PI/4 || mOrientation.z < - 3*(float)Math.PI/4)
                status = false;
            mPosition.x = Math.round(point.x) + 0.5f + HBradius;
        }
        // Stay away from front left wall
        localPoint = new Vector3f(DIRECTION_AHEAD_LEFT);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
    
        // Stay away from front right wall
        localPoint = new Vector3f(DIRECTION_AHEAD_RIGHT);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        
        // Northeast
        point = new Vector3f(DIRECTION_NORTHEAST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Southeast
        point = new Vector3f(DIRECTION_SOUTHEAST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Southwest
        point = new Vector3f(DIRECTION_SOUTHWEST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Northwest
        point = new Vector3f(DIRECTION_NORTHWEST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);

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

        // Eat an edible square
		if (m_env.affordEat(mPosition)) 
		{
			m_eat = true;
			suck();
		}
        
        // Eat a fish agent.
        if (affordHunt(point))
        {
			m_eat = true;
            m_env.removeEntity(point, mName);
        }

        if (!status)
        	m_bump = true;
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
                g2d.fill(somatoMap[i][j]);
            }
        
        // Draw the focus
        
        focusColor = new Color(m_ernest.getAttention());
        g2d.setColor(focusColor);
        g2d.fill(focus);
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
    
	private void rotate(Vector3f vector, float angle) 
	{
		Matrix3f rot = new Matrix3f();
		rot.rotZ(angle);		
		rot.transform(vector); // (rot * localVec) is placed into parentVec
	}	
	
	public void paintSpaceMemory(Graphics g)
	{
		final int WIDTH = 300;
		final int HEIGHT = 250;
		final int SCALE = 35;//40; 

		boolean displaySensePlace = true;
		
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

		//IPlace focusPlace = getErnest().getFocusPlace();
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
		
		// Display agent
        AffineTransform ref = g2d.getTransform();
//        AffineTransform placeAgent = new AffineTransform();
//        placeAgent.translate(WIDTH, HEIGHT);
//        placeAgent.rotate(Math.PI/2);
//        placeAgent.scale(SCALE / 100f, SCALE / 100f);
//        g2d.transform(placeAgent);
//		g2d.setColor(Color.gray);
//        g2d.fill(Ernest110Model.shape(spaceMemory.getID()));
        
        g2d.setTransform(ref);
        
		double d;
		double rad;
		double angle;
		double span;
		
        // Display the places
		//g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
		g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER));

		Ellipse2D.Double circle = new Ellipse2D.Double(-10, -10, 20, 20); 
        Arc2D.Double pie = new Arc2D.Double(-10, -10, 20, 20,0, 180, Arc2D.PIE);
        Polygon triangle = new Polygon();triangle.addPoint(-10,-10);triangle.addPoint(-10,10);triangle.addPoint(10,0);

//        if (displaySensePlace)
//        {
//			// Display the visual and tactile places
//			for (IPlace place : getErnest().getPlaceList())
//			{
//				//if (place.getType() == Spas.PLACE_PERSISTENT)
//				if (place.getType() < Spas.PLACE_FOCUS && place.getType() > Spas.PLACE_BACKGROUND)
//				{
//					d = place.getPosition().length() * SCALE;
//					
//					rad = (float)Math.atan2((double)place.getPosition().y, place.getPosition().x);			
//					//rad = (float)Math.atan2((double)place.getFirstPosition().y, place.getFirstPosition().x);			
//					angle = rad*180/Math.PI;
//								
//					span=place.getSpan()*180/Math.PI;
//					g2d.setColor(new Color(place.getBundle().getValue()));		
//					
//					//g2d.setStroke(new BasicStroke(SCALE / (3f + 2*(spaceMemory.getUpdateCount() - place.getUpdateCount())), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
//					g2d.setStroke(new BasicStroke(Math.max(SCALE / 4f * ( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
//		
//					if (place.getType() == Spas.PLACE_TOUCH)
//						//g2d.drawArc(WIDTH - (int)d, HEIGHT - (int)d, 2*(int)d, 2*(int)d, (int)(angle), (int)1);
//						g2d.drawArc(WIDTH - (int)d, HEIGHT - (int)d, 2*(int)d, 2*(int)d, (int)(ErnestUtils.polarAngle(place.getFirstPosition())*180/Math.PI), (int)(place.getSpan()*180/Math.PI));
//					else
//						g2d.drawLine(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE), 
//							WIDTH + (int)(place.getSecondPosition().x * SCALE), HEIGHT - (int)(place.getSecondPosition().y * SCALE));
//					
//				}			
//			}
//        }
//		
//		// Display the bump, eat, and cuddle places
//		g2d.setStroke(new BasicStroke(SCALE / 20f));
//		AffineTransform or;
//		for (IPlace place : getErnest().getPlaceList())
//		{
//			if (place.getType() >= Spas.PLACE_FOCUS)// && place.getType() < Spas.PLACE_PERSISTENT)
//			{
//				// The places represented as arcs
//				//g2d.setColor(new Color(place.getBundle().getValue()));		
//				if (place.getType() == Spas.PLACE_BUMP) 
//					g2d.setColor(Color.RED);
//				if (place.getType() == Spas.PLACE_EAT )
//					g2d.setColor(Color.YELLOW);
//				if (place.getType() == Spas.PLACE_CUDDLE) 
//					g2d.setColor(Color.PINK);
//				if (place.getType() == Spas.PLACE_PRIMITIVE) 
//					g2d.setColor(Color.BLUE);
//				if (place.getType() == Spas.PLACE_COMPOSITE) 
//					g2d.setColor(new Color(0, 0, 128));
//				if (place.getType() == Spas.PLACE_INTERMEDIARY) 
//					g2d.setColor(new Color(128, 128, 255));
//				if (place.getType() == Spas.PLACE_FOCUS) 
//					g2d.setColor(Color.MAGENTA);
//				
//				Shape shape = circle;
//				if (place.getShape() == Spas.SHAPE_TRIANGLE)
//					shape = triangle;
//				else if (place.getShape() == Spas.SHAPE_PIE)
//					shape = pie;
//				
//		        ref = g2d.getTransform();
//		        or = new AffineTransform();
//		        or.translate(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE));
//		        or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
//		        or.rotate(- place.getOrientation());
//		        g2d.transform(or);
//				g2d.fill(shape);
//		        g2d.setTransform(ref);
//				//g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
//	
//				//g2d.drawLine(WIDTH + (int)(place.getFirstPosition().x * SCALE), HEIGHT - (int)(place.getFirstPosition().y * SCALE), 
//				//	WIDTH + (int)(place.getSecondPosition().x * SCALE), HEIGHT - (int)(place.getSecondPosition().y * SCALE));
//			}
//		}
				
//		// Display the focus
//		int focusRadius = SCALE / 5;
//		g2d.setStroke(new BasicStroke(SCALE / 10f));
//
//		d = focusPlace.getPosition().length() * SCALE;
//		rad = (float)Math.atan2((double)focusPlace.getPosition().y, focusPlace.getPosition().x);			
//		g2d.setColor(new Color(focusPlace.getBundle().getValue()));		
//		int x0 = WIDTH + (int) (d * Math.cos(rad));
//		int y0 = HEIGHT - (int) (d * Math.sin(rad));
//		//g2d.fillOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
//		
//		g2d.setColor(Color.MAGENTA);	
//		//g2d.setColor(new Color(focusPlace.getBundle().getValue()));
//		if (focusPlace.getSpeed() != null)
//			g2d.drawLine(x0, y0, x0 + (int)(focusPlace.getSpeed().x * SCALE), y0 - (int)(focusPlace.getSpeed().y * SCALE));
//		g2d.setStroke(new BasicStroke(SCALE / 20f));
//		g2d.drawOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
//		//g2d.fillOval(x0 - focusRadius, y0 - focusRadius, 2 * focusRadius, 2 * focusRadius);
//		//g2d.setColor(Color.BLUE);	
//		float absoluteOrientation = focusPlace.getOrientation();// + agentOrientation; 
//		//g2d.drawLine(x0 - (int)(Math.cos(absoluteOrientation) * focusRadius), y0 + (int)(Math.sin(absoluteOrientation) * focusRadius), x0 + (int)(Math.cos(absoluteOrientation) * focusRadius *2), y0 - (int)(Math.sin(absoluteOrientation) * focusRadius *2));

		// Display agent
        //AffineTransform ref = g2d.getTransform();
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
