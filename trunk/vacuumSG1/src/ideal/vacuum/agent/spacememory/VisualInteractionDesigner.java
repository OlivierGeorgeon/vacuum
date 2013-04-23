package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.behavior.BehaviorState ;
import ideal.vacuum.agent.vision.Eyes ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;

import javax.vecmath.Point3f ;

import spas.IPlace ;
import spas.LocalSpaceMemory ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class VisualInteractionDesigner extends AbstractSMInteractionDesigner {

	private Graphics2D g2d ;
	private SpaceMemoryMove smMove ;
	private SpaceMemoryTactileEffect smTactileEffect ;
	private SpaceMemoryVisualEffect smVisualEffect ;

	@Override
	public void addInteraction( Graphics2D g2d , IPlace place , BehaviorState behaviorState ) {
		this.g2d = g2d ;
		String interactionLabel = place.getInteraction().getLabel() ;
		this.smMove = SpaceMemoryMove.getSpaceMemoryMove( SpaceMemoryMove
				.extractMoveLabel( interactionLabel ) ) ;
		this.smTactileEffect = SpaceMemoryTactileEffect
				.getSpaceMemoryTactileEffect( SpaceMemoryTactileEffect
						.extractTactileEffectLabel( interactionLabel ) ) ;
		this.smVisualEffect = SpaceMemoryVisualEffect
				.getSpaceMemoryVisualEffect( SpaceMemoryVisualEffect
						.extractLeftVisualEffectLabel( interactionLabel ) ) ;

		this.applyGeometricalTransformation(
				place.getOrientationAngle() ,
				place.getPosition() ,
				( LocalSpaceMemory.PERSISTENCE_DURATION - place.getClock() ) / 1.5f ) ;
		this.fillAndDrawShape() ;
	}

	private void applyGeometricalTransformation( float orientationAngle , Point3f position ,
			double scale ) {
		AffineTransform interactionLocation = new AffineTransform() ;
		interactionLocation.translate(
				(int) ( position.x * SpaceMemoryDesigner.SCALE ) ,
				-(int) ( position.y * SpaceMemoryDesigner.SCALE ) ) ;
		interactionLocation.rotate( -orientationAngle ) ;
		interactionLocation.scale( scale , scale ) ;
		this.g2d.transform( interactionLocation ) ;
	}

	private void fillAndDrawShape() {
		this.g2d.setColor( this.smVisualEffect.getEffectColor() ) ;
		this.g2d.fill( this.smMove.getShape() ) ;
		this.g2d.setColor( Color.BLACK ) ;
		this.g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 20f ) ) ;
		// this.g2d.draw( this.smMove.getShape() ) ;
	}
}
