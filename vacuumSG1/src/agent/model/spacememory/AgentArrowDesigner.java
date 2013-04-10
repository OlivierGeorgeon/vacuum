package agent.model.spacememory;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;

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

		Area agent = AgentArrowDesigner.arrowBodyShape() ;
		g2d.setColor( agentColor ) ;
		g2d.fill( agent ) ;
		g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 10f ) ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.draw( agent ) ;
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
}
