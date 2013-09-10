package ideal.vacuum.agent.vision ;

import ideal.vacuum.Environment ;
import ideal.vacuum.ErnestModel ;
import ideal.vacuum.agent.VisualEffect ;
import java.awt.Color ;
import java.util.LinkedList ;
import java.util.Queue ;
import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;
import utils.ErnestUtils;
import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class Eye {

	public static int RESOLUTION_RETINA = 1 ;
	private Environment env;
	private ErnestModel model;
	private String agentName;
	private Queue<PhotoreceptorCell> activePhotoreceptorCells;
	
	public Eye( Environment env , ErnestModel ernestModel , Vector3f startPosition , String agentName ) {
		this.env = env;
		this.model = ernestModel;
		this.agentName = agentName;
		//PhotoreceptorCell cell = new PhotoreceptorCell(
		//		Ernest.INFINITE ,
		//		Ernest.INFINITE ,
		//		Environment.empty.color )  ;
		this.activePhotoreceptorCells = new LinkedList<PhotoreceptorCell>() ;
		//this.activePhotoreceptorCells.add(cell);
	}

	public void activeRetina() {
		System.out.println("vision");
		double yawRad = this.model.getOrientation();
		double angleOrigin = yawRad - Math.PI/2;
		double angleSpan = Math.PI;
		RayTracing cellsTracing = new RayTracing( this.env , this.model , this.agentName , angleOrigin , angleSpan ) ;
		Queue<PhotoreceptorCell> cells = cellsTracing.rayTrace() ;
		
		for ( PhotoreceptorCell photoreceptorCell : cells ) {
			System.out.println("retina (" + photoreceptorCell.getxBlockPosition() + "," + photoreceptorCell.getyBlockPosition() + ")");
		}
		
		// Agent up, left, down
		if ((Math.abs(yawRad - Math.PI/2) < .1f) || (Math.abs(yawRad + Math.PI/2) < .1f) || (Math.abs(Math.PI - yawRad) < .1f || Math.abs(yawRad + Math.PI) < .1f)){
			for ( PhotoreceptorCell photoreceptorCell : cells ) {
				photoreceptorCell.orienteAxis( yawRad );
			}
		}

		this.activePhotoreceptorCells = cells;
	}
	
	public Queue<PhotoreceptorCell> getActivePhotoreceptorCells() {
		return this.activePhotoreceptorCells ;
	}
}
