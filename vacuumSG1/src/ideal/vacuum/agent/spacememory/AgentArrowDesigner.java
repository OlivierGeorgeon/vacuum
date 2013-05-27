package ideal.vacuum.agent.spacememory;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;

import spas.SimuImpl;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class AgentArrowDesigner extends AbstractSMAgentDesigner {

	private Graphics2D g2d;
	
	@Override
	public void addAgent( Graphics2D g2d , Color agentColor) {
		this.g2d = g2d;
		this.applyGeometricalTransformation() ;

		g2d.setColor( agentColor ) ;
		g2d.fill( AgentArrowDesigner.arrowBodyShape() ) ;
		g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 10f ) ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.draw( AgentArrowDesigner.arrowBodyShape() ) ;
		g2d.setColor( Color.GRAY );
		g2d.draw( AgentArrowDesigner.fieldOfVision() );
	}

	private void applyGeometricalTransformation() {
		AffineTransform agentLocation = new AffineTransform() ;
		agentLocation.scale( SpaceMemoryDesigner.SCALE / 100f , SpaceMemoryDesigner.SCALE / 100f ) ;
		this.g2d.transform( agentLocation ) ;
	}

	private static Area arrowBodyShape() {
		GeneralPath body = new GeneralPath() ;
		body.append( new Line2D.Double( -50 , -50 , -30 , 0 ) , false ) ;
		body.append( new Line2D.Double( -30 , 0 , -50 , 50 ) , true ) ;
		body.append( new Line2D.Double( -50 , 50 , 50 , 0 ) , true ) ;
		return new Area( body ) ;
	}
	
	private static Area fieldOfVision() {
		GeneralPath fieldOfVision = new GeneralPath() ;
		fieldOfVision.append( new Ellipse2D.Double( -20 * SpaceMemoryDesigner.SCALE / SimuImpl.SCALE, -20 * SpaceMemoryDesigner.SCALE / SimuImpl.SCALE, 40 * SpaceMemoryDesigner.SCALE / SimuImpl.SCALE, 40 * SpaceMemoryDesigner.SCALE / SimuImpl.SCALE), false );
		return new Area( fieldOfVision );
	}
}
