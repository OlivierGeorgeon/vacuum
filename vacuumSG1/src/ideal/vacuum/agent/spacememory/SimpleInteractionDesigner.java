package ideal.vacuum.agent.spacememory;

import ideal.vacuum.agent.behavior.BehaviorState ;

import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Point2D ;

import javax.vecmath.Point3f ;

import eca.ActInstance;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 * @deprecated use {@link VisualInteractionDesigner}
 */
@Deprecated
public class SimpleInteractionDesigner extends AbstractSMInteractionDesigner {

	private Graphics2D g2d;
	private SpaceMemoryMove smMove ;

	@Override
	public void addInteraction( Graphics2D g2d , ActInstance actInstance, BehaviorState behaviorState  ) {
		this.g2d = g2d;
		//String interactionLabel = place.getPrimitive().getLabel() ;
		String interactionLabel = actInstance.getDisplayLabel() ;
		this.smMove = SpaceMemoryMove.getSpaceMemoryMove( SpaceMemoryMove.extractMoveLabel( interactionLabel ) ) ;
		
		this.applyGeometricalTransformation( actInstance.getOrientationAngle() , actInstance.getPosition() ) ;
		this.fillAndDrawShape( new Color( actInstance.getValue()) );
	}

	private void applyGeometricalTransformation( float orientationAngle , Point3f position ) {
		Point2D cartesianOffset = this.getCartesianOffset( this.smMove , -orientationAngle);
		
		AffineTransform interactionLocation = new AffineTransform() ;
		interactionLocation.translate(
				(int) ( position.x * SpaceMemoryDesigner.SCALE + cartesianOffset.getX() ) ,
				-(int) ( position.y * SpaceMemoryDesigner.SCALE + cartesianOffset.getY() ) ) ;
		interactionLocation.rotate( -orientationAngle ) ;
		this.g2d.transform( interactionLocation ) ;
	}
	
	private void fillAndDrawShape( Color color ) {
		this.g2d.setColor( color ) ;
		this.g2d.fill( this.smMove.getShape() ) ;
		this.g2d.setColor( Color.black ) ;
		//this.g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 20f ) ) ;
		this.g2d.draw( this.smMove.getShape() ) ;
	}
}
