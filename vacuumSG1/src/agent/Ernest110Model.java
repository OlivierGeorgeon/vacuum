package agent;

import javax.vecmath.Vector3f;

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

import ernest.*;
import tracing.*;
import utils.Pair;


/**************************************
 * A Model for Ernest 10.4
 * Ernest gives impulsions for moving forward and turning
 * @author ogeorgeon
 **************************************/
public class Ernest110Model extends ErnestModel 
{
//      final static float TRANSLATION_IMPULSION = .15f; // .13f
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
//      final static float ROTATION_IMPULSION = 0.123f; //(float)Math.toRadians(7f); // degrees   . 5.5f
    final static float ROTATION_FRICTION = .9f; // .95f

    private static Color UNANIMATED_COLOR = Color.GRAY;
    
    private boolean tempo=true;
    private int lastAction;
    private boolean status;
    
    private Vector3f mPreviousPosition = new Vector3f(mPosition);
    
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
//              setEyeAngle(Math.PI/4);
//              setOrientationStep(5);
        
        status=true;

        setChanged();
        notifyObservers2();                                     
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
        
        // Only trace the first agent.
        
        if (ident == 1)
        	m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","h-yXVWrEwtYclxuPQUlmTOXprcFzol");
                        
        // Initialize the Ernest === 
        
        m_ernest.setParameters(5, 4);
        m_ernest.setTracer(m_tracer);
        m_ernest.setSensorymotorSystem(m_sensorymotorSystem);

        // Ernest's inborn primitive interactions
        
        m_ernest.addInteraction(">", " ",   20); // Move
        m_ernest.addInteraction("^", " ",  -10); // Left toward empty
        m_ernest.addInteraction("v", " ",  -10); // Right toward empty
        
        System.out.println("Ernest initialized") ;
    }
    
    /**
     * Initialize the agent's parameters
     */
    protected void initAgent()
    {
    }

    /**
     * Update the agent when the environment is refreshed.
     * (not necessarily a cognitive step for the agent).
     */
    public void update()
    {
        float vlmin = .05f; // 0.1f;
        float vrmin = .05f; // 0.002f;
        
        Vector3f speed = new Vector3f(mPosition);
        speed.sub(mPreviousPosition);
        int v1 = (int)(speed.length() * 1000);
        int v2 = (int)(mTranslation.length() * 1000);
        int t = (int)(mRotation.z * 1000); // TODO take non voluntary rotation into account.
        
        mPreviousPosition.set(mPosition);

		int[][] sense = sense(status);
		sense[2][8] = v1;
		sense[3][8] = t;
		sense[4][8] = v2;
		int[] intention = m_ernest.update(sense);
		
		enactSchema(intention);

        status = anim();
    }
    
    /**
     * Execute a cognitive step for Ernest.
     */
    private int[][] sense(boolean status)
    {
        if (m_tracer != null)
                m_tracer.startNewEvent(m_counter);

        // See the environment
        // 12 visual pixels * 8 visual info + 1 miscelaneous + 3 tactile
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
        for (int i = 0; i < 9; i++)
                matrix[i][9] = somatoMap[i];
        
        return matrix;
    }
    
    /**
     * Execute a cognitive step for Ernest.
     */
    public int[] stepErnest(boolean status)
    {
        if (m_tracer != null)
                m_tracer.startNewEvent(m_counter);

        // See the environment
        // 12 visual pixels * 8 visual info + 1 miscelaneous + 3 tactile
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
        for (int i = 0; i < 9; i++)
                matrix[i][9] = somatoMap[i];
        
        return m_ernest.step(matrix);
    }
    
    /**
     * Enact the primitive schema chosen by Ernest.
     * @return binary feedback. 
     */
    public boolean enactSchema(int[] schema)
    {
    	if (schema[0] != 0)
    	{
	        //m_schema = schema;
	        m_schema = Character.toString((char)schema[0]);
	        int impulsion = schema[1];
	
	        // A new interaction cycle is starting
	        m_counter++;
	        System.out.println("Agent #"+ident+", Step #" + m_counter + "=======");
	        
	        if (schema[0] == 'v')
	        	mRotation.add(new Vector3f(0, 0, - impulsion / 1000f));
	            //mRotation.add(new Vector3f(0, 0, - ROTATION_IMPULSION));
	        else if (schema[0] == '^')
	        	mRotation.add(new Vector3f(0, 0, impulsion / 1000f));
	        else if (schema[0] == '>')
	            mTranslation.add(new Vector3f(impulsion / 1000f, 0, 0));
	            //mTranslation.add(new Vector3f(TRANSLATION_IMPULSION, 0, 0));
	
	        // Trace the environmental data
	        if (m_tracer != null)
	        {
	                Object environment = m_tracer.newEvent("environment", "position", m_counter);
	                m_tracer.addSubelement(environment, "x", mPosition.x + "");
	                m_tracer.addSubelement(environment, "y", mPosition.y + "");
	                m_tracer.addSubelement(environment,"orientation", mOrientation.z + "");
	        }               
    	}
        return true;
    }

    /**
     * Taste the square where Ernest is. 
     * Suck the square if it is food or water. 
     * @return 0 if nothing, 1 if water, 2 if food. 
     */
    private int taste()
    {
		int taste = Ernest.STIMULATION_GUSTATORY_NOTHING;
		Vector3f point = new Vector3f(DIRECTION_AHEAD);
		point.scale(TACTILE_RADIUS);

		// Sucking water or food if any
		if (affordEat(mPosition)) 
		{
			suck();
			taste = Ernest.STIMULATION_GUSTATORY_FISH;
		}
		// if no food then check for cuddle
		else if (affordCuddle(localToParentRef(point))) // mCuddled
		{
			taste = Ernest.STIMULATION_GUSTATORY_CUDDLE;
		}
//		else if (affordCuddle(localToParentRef(DIRECTION_AHEAD_RIGHT)) ||
//				affordCuddle(localToParentRef(DIRECTION_AHEAD_LEFT)))
//			SoundManager.cuddle.play();

		return taste;
    }
    
    /**
     * Perform an action in the environment 
     * @param act the action to perform
     * @return true if bump, false if not bump. 
     */
    private boolean anim()
    {
        boolean status = true;
        float HBradius = BOUNDING_RADIUS;  // radius of Ernest hitbox 
        
        mPosition.set(localToParentRef(mTranslation));
        mOrientation.z += mRotation.z;
        if (mOrientation.z < - Math.PI) 
        {
        	mOrientation.z += 2 * Math.PI;
        }
        if (mOrientation.z > Math.PI) 
        {
        	mOrientation.z -= 2 * Math.PI;
        }
        
        // Bumping ====

        // Stay away from north wall
        Vector3f point = new Vector3f(DIRECTION_NORTH);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
        {
            if (mOrientation.z > (float)Math.PI/4 && mOrientation.z < 3*(float)Math.PI/4)
                // It counts as a bump only if the angle is closer to perpendicular plus or minus PI/4
                status = false;
            mPosition.y = Math.round(point.y) - 0.5f - HBradius;
        }
        // Stay away from east wall
        point = new Vector3f(DIRECTION_EAST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
        {
            if (mOrientation.z > - (float)Math.PI/4 && mOrientation.z < (float)Math.PI/4) 
                status = false;
            mPosition.x = Math.round(point.x) - 0.5f - HBradius;
        }
        // Stay away from south wall
        point = new Vector3f(DIRECTION_SOUTH);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
        {
            if (mOrientation.z < - (float)Math.PI/4 && mOrientation.z > - 3 *(float)Math.PI/4)
                status = false;
            mPosition.y = Math.round(point.y) + 0.5f + HBradius;
        }
        // Stay away from west wall
        point = new Vector3f(DIRECTION_WEST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
        {
            if (mOrientation.z > 3*(float)Math.PI/4 || mOrientation.z < - 3*(float)Math.PI/4)
                status = false;
            mPosition.x = Math.round(point.x) + 0.5f + HBradius;
        }
        // Stay away from ahead left wall
        Vector3f localPoint = new Vector3f(DIRECTION_AHEAD_LEFT);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
    
        // Stay away from Ahead right wall
        localPoint = new Vector3f(DIRECTION_AHEAD_RIGHT);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        
        // Northeast
        point = new Vector3f(DIRECTION_NORTHEAST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Southeast
        point = new Vector3f(DIRECTION_SOUTHEAST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Southwest
        point = new Vector3f(DIRECTION_SOUTHWEST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);
        // Northwest
        point = new Vector3f(DIRECTION_NORTHWEST);
        point.scaleAdd(HBradius, mPosition);
        if (!affordWalk(point))
            keepDistance(mPosition, cellCenter(point), HBradius + .5f);

        // Stay away from agent ahead
        localPoint = new Vector3f(DIRECTION_AHEAD);
        localPoint.scale(HBradius);
        point = localToParentRef(localPoint);
        if (affordCuddle(point))
        {
            keepDistance(mPosition, entityCenter(point), 2 * HBradius ); // Allow some overlap
            if (!mCuddled)
                mTranslation.scale(.5f); // slowing down makes it look more like cuddling.
            setCuddled(true);
        }
        
        // Apply friction to the speed vectors
        mTranslation.scale(TRANSLATION_FRICTION);
        mRotation.scale(ROTATION_FRICTION);
        
        mainFrame.drawGrid();
        
        return status;
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
        }
        
        // The shark body

        Area shark = shark();
        
        // Retina pixel
        
        Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
        
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
     * @return The shark area.
     */
    private Area shark()
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
        shark.add(new Area(leftPectoralFin));shark.add(new Area(leftPelvicFin));
        shark.add(new Area(rightPectoralFin));shark.add(new Area(rightPelvicFin));
        
        return shark;
    }
}
