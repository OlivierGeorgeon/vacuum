package ideal.vacuum.agent.vision;

import ideal.vacuum.Environment ;
import ideal.vacuum.ErnestModel ;
import ideal.vacuum.Model ;

import java.awt.Color ;
import java.util.LinkedList ;
import java.util.Queue ;

import javax.vecmath.Vector3f ;

import ernest.Ernest ;

public class RayTracing {

	private Environment env;
	private ErnestModel model;
	private Vector3f position;
	private String agentName;
	
	private double angleOrigin;
	private double angleSpan;
	private double currentAngle;
	
	public RayTracing( Environment env , ErnestModel ernestModel , Vector3f startPosition , String agentName , double angleOrigin, double angleSpan) {
		this.env = env;
		this.model = ernestModel;
		this.position = startPosition;
		this.agentName = agentName;

		this.angleOrigin = angleOrigin;
		this.angleSpan = angleSpan;
	}
	
	public Queue<PhotoreceptorCell> rayTrace(){
		this.currentAngle = this.angleOrigin;
		Queue<PhotoreceptorCell> cellQueue = new LinkedList<PhotoreceptorCell>() ;
		while( this.currentAngle < this.angleOrigin + this.angleSpan + .001 ){
			PhotoreceptorCell cell = this.scanArc() ;
			if( ! cellQueue.contains( cell ) ){
				cellQueue.add( cell );
			}
		}
		return cellQueue;
	}
	
	private PhotoreceptorCell scanArc() {
		int[] eyeFixation = null; //new int[] {Ernest.INFINITE,Ernest.INFINITE,WALL_COLOR.getRGB()};
		double step = this.angleSpan / 40; // OG
		double angle;
		for (angle = this.currentAngle; angle <= this.angleOrigin + this.angleSpan + .001; angle += step) {
			float x0 = (float) (this.position.x + 20 * Math.cos(angle));
			float y0 = (float) (this.position.y + 20 * Math.sin(angle)); // Y axis is downwards.
			eyeFixation = rayTrace(this.position.x,this.position.y, x0, y0);
			// We stop when we find a singularity.
			if (eyeFixation[2] != Model.WALL_COLOR.getRGB()){
				break;
			}
		}
		this.currentAngle = angle + step;
		
		return new PhotoreceptorCell( eyeFixation[0] , eyeFixation[1] , new Color( eyeFixation[2] ) );
	}
	
	/**
	 * Scan the squares that are on a ray from a viewpoint to a target square
	 *  http://playtechs.blogspot.com/2007/03/raytracing-on-grid.html 
	 * @return Distance to the dirty square if any, Ernest.INFINITE if no dirt. 
	 */
	//protected Pair<Integer, Color> rayTrace(float x0, float y0, float x1, float y1) {
	private int[] rayTrace(float x0, float y0, float x1, float y1) {
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
	    	if ( this.model.isOutOfBoard( i , j ) ) 
	    		//return Pair.create(Ernest.INFINITE, WALL_COLOR);
	    		return new int[] {Ernest.INFINITE,Ernest.INFINITE,Model.WALL_COLOR.getRGB()};
	    	
	    	// Examine the block on the ray. Return wall or uninhibited dirty squares.
	    	Color bgc = this.env.m_blocks[i][j].seeBlock();
	    	if (bgc.equals(Model.WALL_COLOR)) // don't see walls (for Ernest 11.4)
	    		//return Pair.create(Ernest.INFINITE, WALL_COLOR);
	    		return new int[] {Ernest.INFINITE,Ernest.INFINITE,Model.WALL_COLOR.getRGB()};
	    	
	    	if (this.env.isWall(i,j) || this.env.isFood(i,j) || this.env.isAlga(i,j))
	    	{
				//int dist = (int) Math.sqrt(((i-x0)*(i-x0) + (j-y0)*(j-y0)) * Ernest.INT_FACTOR * Ernest.INT_FACTOR);
				//return Pair.create(dist, bgc);
	    		return new int[] {(i-(int)Math.round(x0)) , (j-(int)Math.round(y0)) , bgc.getRGB()};

    		}
	    	//if (m_env.isAgent(i, j, mName))
	    	ErnestModel entity = this.env.getEntity(new Vector3f(i,j,0), this.agentName);
	    	if (entity != null)
	    	{
				//int dist = (int) Math.sqrt(((i-x0)*(i-x0) + (j-y0)*(j-y0)) * Ernest.INT_FACTOR * Ernest.INT_FACTOR);
				//return Pair.create(dist, entity.getColor());//AGENT_COLOR);
	    		return new int[] {(i-(int)Math.round(x0)) , (j-(int)Math.round(y0)) , -entity.getColor().getRGB()};
	    	}

	    }
		//return Pair.create(Ernest.INFINITE, WALL_COLOR);
		return new int[] {Ernest.INFINITE,Ernest.INFINITE,Model.WALL_COLOR.getRGB()};
	}
}
