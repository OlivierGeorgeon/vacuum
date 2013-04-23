package ideal.vacuum;

import ideal.vacuum.agent.vision.Eyes ;

import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.CubicCurve2D ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;

import javax.vecmath.Vector3f ;


/**************************************
 * A Model for Ernest 10.4
 * Ernest gives impulsions for moving forward and turning
 * @author ogeorgeon
 **************************************/
public class FishModel extends ErnestModel 
{
	private final static Color AGENT_COLOR = new Color(150,128,255);
	
	final static float TRANSLATION_SPEED = .1f; //.15f
	
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
    final static float ROTATION_FRICTION = .9f; // .95f

    Color[] pixelColor = new Color[Eyes.RESOLUTION_RETINA];
    Color[][] somatoMapColor = new Color[3][3];

    public float[] fish_x={(float) 0.8,(float) 0.6,(float) 0.4,(float) 0.2,0,(float)-0.2,(float)-0.4,(float)-0.6,(float)-0.8,
            (float)-0.8,(float)-0.6,(float)-0.4,(float)-0.2,0,(float) 0.2,(float) 0.4,(float) 0.6,(float) 0.8};
	public float[] fish_y={0,(float) 0.4,(float) 0.5,(float) 0.5,(float) 0.5,(float) 0.4,(float) 0.3,0,(float) 0.5,
            (float)-0.5,0,(float)-0.3,(float)-0.4,(float)-0.5,(float)-0.5,(float)-0.5,(float)-0.4,0};
	private GeneralPath m_fish = new GeneralPath();

	private int m_interactionTimer = 0;
	
	private static int INTERACTION_PERIOD = 15;
    
    /**
     * @param i The agent's numerical id. 
     */
    public FishModel(int i) 
    {
        super(i);
        mName = "Fish " + i;
    }
    
    /**
     * Initialize the agent in the grid
     */
    public void init(int w,int h) throws Exception
    {
        // Initialize the model
        super.init(w,h);
        
		m_fish.append(new CubicCurve2D.Double(-40, 15,  -30, 0, 40, -40,   40, 0), false);
		m_fish.append(new CubicCurve2D.Double( 40,  0,  40, 40, -30,  0,   -40, -15), true);
		m_fish.append(new Area(new Ellipse2D.Double( 20,  -12,  8, 8)), false);
		//m_fish.append(new Area(new Ellipse2D.Double( 20,  4,  8, 8)), false);
		
        setChanged();
        notifyObservers2();          
    }

    /**
     * @return The version of the Ernest model
     */
    public String getVersion()
    {
        return "Fish 0";
    }
    
    /**
     * Initialize the fish.
     */
    public void initErnest()
    {

        cognitiveMode = AGENT_RUN;
        mTranslation = new Vector3f();
        mRotation  = new Vector3f();
        
		mTranslation = new Vector3f(TRANSLATION_SPEED, 0,0);
        
       System.out.println("Fish initialized") ;
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
    	m_interactionTimer++;
    	
//    	if (m_interactionTimer > INTERACTION_PERIOD)
//    	{
//    		m_interactionTimer = 0;
//            mTranslation.add(new Vector3f((float) SpatialSensorimotorSystem.TRANSLATION_IMPULSION, 0, 0));
//    	}

    	// compute absolute movements
		mSpeedT=new Vector3f(mPosition);
		mSpeedT.sub(mPreviousPosition);
		
		mSpeedR=new Vector3f(mOrientation);
		mSpeedR.sub(mPreviousOrientation);
		
		if (mSpeedR.z > Math.PI) mSpeedR.z-=2*Math.PI;
		if (mSpeedR.z<=-Math.PI) mSpeedR.z+=2*Math.PI;
		
		mPreviousPosition.set(mPosition);
		mPreviousOrientation.set(mOrientation);
        
       if (cognitiveMode == AGENT_STEP)
        	cognitiveMode = AGENT_STOP;

        anim();
    }
    

    /**
     * Animate the agent in the environment 
     */
    private void anim()
    {
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
        	mOrientation.z = - mOrientation.z;
            mPosition.y = Math.round(point.y) - 0.5f - HBradius;
        }
        // Stay away from east wall
        point = new Vector3f(DIRECTION_EAST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
        	mOrientation.z = (float)Math.PI - mOrientation.z;
            mPosition.x = Math.round(point.x) - 0.5f - HBradius;
        }
        // Stay away from south wall
        point = new Vector3f(DIRECTION_SOUTH);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
        	mOrientation.z = - mOrientation.z;
            mPosition.y = Math.round(point.y) + 0.5f + HBradius;
        }
        // Stay away from west wall
        point = new Vector3f(DIRECTION_WEST);
        point.scaleAdd(HBradius, mPosition);
        if (!m_env.affordWalk(point))
        {
        	mOrientation.z = (float)Math.PI - mOrientation.z;
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

        // Apply friction to the speed vectors
        //mTranslation.scale(TRANSLATION_FRICTION);
        mRotation.scale(ROTATION_FRICTION);
        
        mainFrame.drawGrid();        
    }

    /**
     * Paint the fish.
     * @param g The graphic object for painting.
     */
    public void paintAgent(Graphics2D g2d,int x,int y,double sx,double sy)
    {
        // The orientation

        AffineTransform orientation = new AffineTransform();
        orientation.translate(x,y);
        orientation.rotate(-mOrientation.z);
        orientation.scale(sx,sy);
        g2d.transform(orientation);

        // Paint the fish
		g2d.setColor(Environment.FISH1);
		g2d.fill(m_fish);
    }
    
    public Color getColor()
    {
    	return AGENT_COLOR;
    }
    
    public boolean affordEat()
    {
    	return true;
    }
    
    public boolean affordCuddle()
    {
    	return false;
    }
    
    public boolean isAgent(){
    	return false;
    }
		
	public int getCounter()
	{
		return m_interactionTimer / INTERACTION_PERIOD;
	}

}
