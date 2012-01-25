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
import java.util.List;

import ernest.*;
import spas.IPlace;
import tracing.*;
import utils.Pair;

/**************************************
 * A Model for a fish that moves in the environment
 * @author ogeorgeon
 **************************************/
public class Ernest110Model extends ErnestModel 
{
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
    final static float ROTATION_FRICTION = .9f; // .95f

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
        m_ernest = new Ernest();
        m_sensorymotorSystem = new SpatialSensorimotorSystem();
		m_SpaceMemory.setErnest(m_ernest);
		//m_SpaceMemory.setModel(this);
        
        // Only trace the first agent.
        
        if (ident == 0)
        	m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","h-yXVWrEwtYclxuPQUlmTOXprcFzol");
                        
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
	}
    
    
    
    /**
     * Update the agent when the environment is refreshed.
     * (not necessarily a cognitive step for the agent).
     */
    public void update()
    {

    	// Sense the environment on each update.
		int[][] sense = sense();
    	
    	// The movement since the last update.
    	
        Vector3f move = new Vector3f(mPosition);
        move.sub(mPreviousPosition);
        
        // The movement relative to Ernest
		Matrix3f rot = new Matrix3f();
		rot.rotZ(- mOrientation.z);
		//Vector3f oldPosition = move;
		rot.transform(move, move); // (rot * move) is placed into move
    
        sense[2][8] = (int)(move.x * Ernest.INT_FACTOR);
        sense[3][8] = (int)(move.y * Ernest.INT_FACTOR);
        sense[4][8] = (int)(mRotation.z * Ernest.INT_FACTOR); // TODO take involuntary rotation into account.
        sense[5][8] = (int)(mTranslation.length() * Ernest.INT_FACTOR);
        sense[6][8] = cognitiveMode;
        
        
        
     // compute absolute movements
		mSpeedT=new Vector3f(mPosition);
		mSpeedT.sub(mPreviousPosition);
		
		mSpeedR=new Vector3f(mOrientation);
		mSpeedR.sub(mPreviousOrientation);
		
		if (mSpeedR.z > Math.PI) mSpeedR.z-=2*Math.PI;
		if (mSpeedR.z<=-Math.PI) mSpeedR.z+=2*Math.PI;
		
		mPreviousPosition.set(mPosition);
		mPreviousOrientation.set(mOrientation);
        
        // Update Ernest.
		
		m_ernest.setSegmentList(m_eye.segments);
        
		int[] intention = m_ernest.update(sense);
		
        if (intention[0] != 0 && cognitiveMode == AGENT_STEP)
        	cognitiveMode = AGENT_STOP;

		enactSchema(intention);
		
		// Refresh the local space memory window
		//m_SpaceMemory.update( (ArrayList<IPlace>) getPlaceList() );
		//m_SpaceMemory.update( (ArrayList<IPlace>) m_ernest.getPlaceList());
		m_SpaceMemory.update();
		for (int i=0;i<m_env.frameList.size();i++){
			m_env.frameList.get(i).repaint();
		}
		
		if (cognitiveMode!=AGENT_STOP) rendu();
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
        
        // Tactile ===
        
        int [] somatoMap = somatoMap();
        if (m_cuddle)
        	// If Ernest is cuddling then he is also touching ahead.
        	somatoMap[3] = Ernest.STIMULATION_TOUCH_AGENT;
        if (m_eat)
        	// If Ernest is cuddling then he is also touching ahead.
        	somatoMap[3] = Ernest.STIMULATION_TOUCH_FISH;
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
        
        mPosition.set(localToParentRef(mTranslation));
        mOrientation.z += mRotation.z;
        
        if (mOrientation.z < - Math.PI) mOrientation.z += 2 * Math.PI;
        if (mOrientation.z > Math.PI)   mOrientation.z -= 2 * Math.PI;
        
        // Bumping ====

        // Stay away from north wall
        Vector3f point = new Vector3f(DIRECTION_NORTH);
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
        // Stay away from ahead left wall
        Vector3f localPoint = new Vector3f(DIRECTION_AHEAD_LEFT);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (!m_env.affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
    
        // Stay away from Ahead right wall
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

        // Eat
		if (m_env.affordEat(mPosition)) 
		{
			m_eat = true;
			suck();
		}
        
        // Cuddle the agent ahead
        localPoint = new Vector3f(DIRECTION_AHEAD);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (affordCuddle(point))
        {
            keepDistance(mPosition, entityCenter(point), 2 * HBradius );
            if (!m_cuddle)
                mTranslation.scale(.5f); // slowing down makes it look more like cuddling.
            m_cuddle = true;
        }
        
        // Eat a fish agent.
        if (affordHunt(point))
        {
			m_eat = true;
            m_env.removeEntity(point, mName);
        }
        
        // Apply friction to the speed vectors
        mTranslation.scale(TRANSLATION_FRICTION);
        mRotation.scale(ROTATION_FRICTION);
        
        mainFrame.drawGrid();
        
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
        
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
            {
                g2d.setColor(somatoMapColor[i][j]);
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
}
