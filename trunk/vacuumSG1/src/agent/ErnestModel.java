package agent;


import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Ellipse2D;
import java.util.ArrayList;

import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;

import ernest.*;
import utils.Pair;
import tracing.*;

/**************************************
 * A Model for Ernest 
 * This class gathers methods that, we believe, will survive generations of Ernests.
 * @author ogeorgeon
 **************************************/
public class ErnestModel extends Model 
{

	public static int ACTION_FORWARD = 0;
	public static int ACTION_LEFT = 1;
	public static int ACTION_RIGHT = 2;
	
	//protected boolean status;
	
	public boolean isStep=false;
	public boolean step=false;
	public boolean run=true;

	/** The angular field of each eye. */
	private double m_eyeAngle ;
	protected void setEyeAngle(double angle) {m_eyeAngle = angle;}
	protected double getEyeAngle() {return m_eyeAngle;}
	
	/** Ernest's sensorymotor system. */
	protected IErnest m_ernest;
	protected ISensorymotorSystem m_sensorymotorSystem;
	protected ITracer m_tracer;

	/**
	 * Value of the diagonal projection in 2D:
	 * 1 for a square diagonal,
	 * 1/sqrt(2) for a circle diagonal.
	 */
	public final static float INV_SQRT_2 = (float) (1/Math.sqrt(2));
	final private float DIAG2D_PROJ = INV_SQRT_2;

	// Local directions
	final public Vector3f DIRECTION_AHEAD = new Vector3f(1, 0, 0);
	final public Vector3f DIRECTION_BEHIND = new Vector3f(-1, 0, 0);
	final public Vector3f DIRECTION_LEFT = new Vector3f(0, 1, 0);
	final public Vector3f DIRECTION_RIGHT = new Vector3f(0, -1, 0);
	final public Vector3f DIRECTION_AHEAD_LEFT = new Vector3f(DIAG2D_PROJ, DIAG2D_PROJ, 0);
	final public Vector3f DIRECTION_AHEAD_RIGHT = new Vector3f(DIAG2D_PROJ, -DIAG2D_PROJ, 0);
	final public Vector3f DIRECTION_BEHIND_LEFT = new Vector3f(-DIAG2D_PROJ, DIAG2D_PROJ, 0);
	final public Vector3f DIRECTION_BEHIND_RIGHT = new Vector3f(-DIAG2D_PROJ, -DIAG2D_PROJ, 0);	
	final public static float SOMATO_RADIUS = 1.1f;
	final public static float TACTILE_RADIUS = .8f;
	
	// Absolute directions in Cartesian coordinates (0,0) bottom left.
	final protected Vector3f DIRECTION_NORTH = new Vector3f(0, 1, 0);
	final protected Vector3f DIRECTION_NORTHEAST = new Vector3f(1, 1, 0);
	final protected Vector3f DIRECTION_EAST = new Vector3f(1, 0, 0);
	final protected Vector3f DIRECTION_SOUTHEAST = new Vector3f(1, -1, 0);
	final protected Vector3f DIRECTION_SOUTH = new Vector3f(0, -1, 0);
	final protected Vector3f DIRECTION_SOUTHWEST = new Vector3f(-1, -1, 0);
	final protected Vector3f DIRECTION_WEST = new Vector3f(-1, 0, 0);
	final protected Vector3f DIRECTION_NORTHWEST = new Vector3f(-1, 1, 0);

	

	public ErnestModel(int i) {
		super(i);
	}
	
	/**
	 * Initialize the Ernest agent.
	 */
	public void initErnest()
	{
	}
	
	public void closeErnest()
	{
		//m_tracer.close();
		mOrientation.z=(float) (Math.PI/2);
		m_ernest = null;
	}
	
	/**
	 * Update the agent when the environment is refreshed.
	 * (not necessarily a cognitive step for the agent).
	 */
	public void update()
	{
//		int[] intention = stepErnest(status);
//		status = enactSchema(intention);
	}
	
	/**
	 * Run Ernest one step
	 */
	public int[] stepErnest(boolean status)
	{

		// Sense the environment
		int [][] matrix = new int [2][1];
		
		//m_sensorymotorSystem.senseMatrix(matrix);
		
		//String intention = m_ernest.step(matrix);
		String intention = Character.toString((char)m_ernest.step(matrix)[0]);

		//return intention;
		return m_ernest.step(matrix);
	}
	
//	public boolean enactSchema(int[] schema)
//	{
//		return true;
//	}
	
	/**
	 * Trace an event generated by the user, typically: a mouse click to change a square on the grid.
	 * @param type The event's type.
	 * @param x The x coordinate on the grid.
	 * @param y The y coordinate on the grid.
	 */
	public void traceUserEvent(String type, int x, int y)
	{
		//Object element = m_tracer.newEvent("user", type, m_counter);
		//m_tracer.addSubelement(element, "x", x + "");
		//m_tracer.addSubelement(element, "y", y + "");
	}
	
	/**
	 * Generates a retina image from Ernest's view point.
	 * (Uses Ernest's orientationRad value, trigonometric, counterclockwise, radius).
	 * @return The array of colors projected onto the retina.
	 */ 
	protected Pair<Integer, Color>[] getRetina(double orientationRad) {
		@SuppressWarnings("unchecked")
		Pair<Integer, Color>[] retina = new Pair[Ernest.RESOLUTION_RETINA];
		double angle = orientationRad - Math.PI/2;
		double angleStep = Math.PI / Ernest.RESOLUTION_RETINA;
		for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++) {
			retina[i] = scanArc((float)angle, (float)angleStep);
			angle += angleStep;
		}
		return retina;
	}
	/**
	 * Scan an arc from Ernest's viewpoint, starting from the initial angle position and going through the angular span.
	 * Stop scanning at the first singularity found.
	 * @param t The initial angular position (trigonometric/counterclockwise - radian)
	 * @param a The arc's angular span (trigonometric/counterclockwise)
	 * @param 20 The arc's diameter (the agent's visual range)
	 * @return the color detected. 
	 */
	protected Pair<Integer, Color> scanArc(float t, float a) {
		Pair<Integer, Color> eyeFixation = null;
		float step = a/2;
		for (float angle = t; angle <= t + a + .001; angle += step) {
			float x0 = (float) (mPosition.x + 20 * Math.cos(angle));
			float y0 = (float) (mPosition.y + 20 * Math.sin(angle)); // Y axis is downwards.
			//float y0 = (float) (m_y + 20 * Math.sin(angle)); // Y axis is upwards.
			eyeFixation = rayTrace(mPosition.x,mPosition.y, x0, y0);
			// We stop when we find a singularity.
			if (eyeFixation.mRight != WALL_COLOR)
				break;
		}
		if (eyeFixation==null)
			return Pair.create(Ernest.INFINITE, WALL_COLOR);
		return eyeFixation;
	}
	/**
	 * Scan the squares that are on a ray from a viewpoint to a target square
	 *  http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html 
	 * @return Distance to the dirty square if any, Ernest.INFINITE if no dirt. 
	 */
	protected Pair<Integer, Color> rayTrace(float x0, float y0, float x1, float y1) {
		float dx = Math.abs(x1 - x0);
		float dy = Math.abs(y1 - y0);
	    int i = (int) Math.round(x0);
	    int j = (int) Math.round(y0);
	    int n = 1;
	    int i_inc, j_inc;
	    float error;
	    //int k = Math.round(mPosition.getZ());
	    float cornerTresh = .05f * dx * dy;

	    if (dx == 0) {
	        i_inc = 0;
	        error = Float.POSITIVE_INFINITY;
	    } else if (x1 > x0) {
	        i_inc = 1;
	        n += (int) Math.round(x1) - i;
	        error = (float) (((Math.round(x0) + .5f) - x0) * dy);
	    } else {
	        i_inc = -1;
	        n += i - (int) Math.round(x1);
	        error = (float) ((x0 - (Math.round(x0) - .5f)) * dy);
	    }
	    if (dy == 0) {
	        j_inc = 0;
	        error -= Float.POSITIVE_INFINITY;
	    } else if (y1 > y0) {
	        j_inc = 1;
	        n += (int) Math.round(y1) - j;
	        error -= ((Math.round(y0) + .5f) - y0) * dx;
	    } else {
	        j_inc = -1;
	        n += j - (int) Math.round(y1);
	        error -= (y0 - (Math.round(y0) - .5f)) * dx;
	    }
	    for (; n > 0; --n) 
	    {
	        // move on along the ray
	        if (error > cornerTresh) {
	            j += j_inc;
	            error -= dx;
	        } else if (error < -cornerTresh) {
	            i += i_inc;
	            error += dy;
	        } else {
	        	i += i_inc;
	    		j += j_inc;
	    		error += dy - dx;
	    		--n;
	        }

	        // Don't go outside the grid
	    	if ((i < 0) || (j < 0) || (i >= m_w) || (j >= m_h)) 
	    		return Pair.create(Ernest.INFINITE, WALL_COLOR);
	    	
	    	// Examine the block on the ray. Return wall or uninhibited dirty squares.
	    	Color bgc = m_env.m_blocks[i][j].seeBlock();
	    	if (m_env.isWall(i,j) || m_env.isFood(i,j) || m_env.isAlga(i,j))
	    	{
		    		int dist = (int)Math.sqrt(((i-x0)*(i-x0) + (j-y0)*(j-y0)) * Ernest.INT_FACTOR);
		    		return Pair.create(dist, bgc);
    		}

	    }
		return Pair.create(Ernest.INFINITE, WALL_COLOR);
	}

	/**
	 * Compute the tactile stimuli 
	 * @return The matrix of tactile stimuli. 
	 */
	protected int[] somatoMap() {
		int[] somatoMap = new int[9];
		somatoMap[0] = soma(DIRECTION_BEHIND_RIGHT);
		somatoMap[1] = soma(DIRECTION_RIGHT);
		somatoMap[2] = soma(DIRECTION_AHEAD_RIGHT);
		somatoMap[3] = soma(DIRECTION_AHEAD);
		somatoMap[4] = soma(DIRECTION_AHEAD_LEFT);
		somatoMap[5] = soma(DIRECTION_LEFT);
		somatoMap[6] = soma(DIRECTION_BEHIND_LEFT);
		somatoMap[7] = soma(DIRECTION_BEHIND);
		somatoMap[8] = soma(new Vector3f());

		return somatoMap;
	}
	/**
	 * Tactile stimuli. 
	 * @param direction The direction of the touch in Ernest's referential.
	 * @return The tactile stimulus in this direction. 
	 */
	protected int soma(Vector3f direction) {
		int soma = Ernest.STIMULATION_TOUCH_EMPTY;
		Vector3f localPoint = new Vector3f(direction);
		localPoint.scale(SOMATO_RADIUS);
		Vector3f point = localToParentRef(localPoint);
		if (affordTouchSoft(point))
			soma = Ernest.STIMULATION_TOUCH_SOFT;
		if (affordEat(point))
			soma = Ernest.STIMULATION_TOUCH_FISH;
		if (!affordWalk(point)) 
			soma = Ernest.STIMULATION_TOUCH_WALL;
		return soma;
	}
	/**
	 * @param localVec A position relative to Ernest.
	 * @return The absolute position relative to the board ((rotZ(mOrientation.z) * localVec) + mPosition). 
	 */
	public Vector3f localToParentRef(Vector3f localVec) {
		
		Matrix3f rot = new Matrix3f();
		rot.rotZ(mOrientation.z);
		
		Vector3f parentVec = new Vector3f();
		rot.transform(localVec, parentVec); // (rot * localVec) is placed into parentVec
		//parentVec.add(new Vector3f(m_x, m_y, 0));
		parentVec.add(mPosition); // now parentVec = (rotZ(mOrientation.z) * localVec) + mPosition.
		return parentVec;
	}	
	
	/**
	 * Ernest's 
	 */
	public void ernestDynamic()
	{
		mTranslation.scale(.9f);
		mPosition.add(mTranslation);
		//m_x = mPosition.x;
		//m_y = m_h - mPosition.y;
		
		mRotation.scale(.9f);
		mOrientation.add(mRotation);
	}
	
	public Vector3f cellCenter(Vector3f position)
	{
		Vector3f cellCenter = new Vector3f(Math.round(position.x), Math.round(position.y), Math.round(position.z));
		return cellCenter;
	}

	public void keepDistance(Vector3f position, Vector3f point, float distance)
	{
		if (point != null)
		{
			Vector3f toPoint = new Vector3f(point);
			toPoint.sub(position);
			if (toPoint.length() < distance)
			{
				//position.add(toPoint);
				position.set(point);
				toPoint.normalize();
				toPoint.scale(- distance);
				position.add(toPoint);
			}
		}
	}
	
	
}
