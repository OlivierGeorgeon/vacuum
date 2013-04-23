package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.behavior.BehaviorState ;

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
	private SpaceMemoryVisualEffect smLeftVisualEffect ;
	private SpaceMemoryVisualEffect smRightVisualEffect ;

	@Override
	public void addInteraction( Graphics2D g2d , IPlace place , BehaviorState behaviorState ) {
		this.g2d = g2d ;
		String interactionLabel = place.getInteraction().getLabel() ;
		this.smMove = SpaceMemoryMove.getSpaceMemoryMove( SpaceMemoryMove
				.extractMoveLabel( interactionLabel ) ) ;
		this.smTactileEffect = SpaceMemoryTactileEffect
				.getSpaceMemoryTactileEffect( SpaceMemoryTactileEffect
						.extractTactileEffectLabel( interactionLabel ) ) ;
		this.smLeftVisualEffect = SpaceMemoryVisualEffect
				.getSpaceMemoryVisualEffect( SpaceMemoryVisualEffect
						.extractLeftVisualEffectLabel( interactionLabel ) ) ;
		this.smRightVisualEffect = SpaceMemoryVisualEffect
				.getSpaceMemoryVisualEffect( SpaceMemoryVisualEffect
						.extractRightVisualEffectLabel( interactionLabel ) ) ;

		this.applyGeometricalTransformation(
				place.getOrientationAngle() ,
				place.getPosition() ,
				( LocalSpaceMemory.PERSISTENCE_DURATION - place.getClock() ) / 1.5f ) ;
		this.fillAndDrawShape() ;
	}

	private void applyGeometricalTransformation( float orientationAngle , Point3f position ,
			double scale ) {
		AffineTransform interactionLocation = new AffineTransform() ;
		float d = (float) Math.sqrt( position.x * position.x + position.y * position.y ) / 4 ; // OG
		if ( d != 0 )
			d = 1 ;
		interactionLocation.translate(
				(int) ( position.x * SpaceMemoryDesigner.SCALE / d ) ,
				-(int) ( position.y * SpaceMemoryDesigner.SCALE / d ) ) ;
		interactionLocation.rotate( -orientationAngle ) ;
		interactionLocation.scale( scale , scale ) ;
		this.g2d.transform( interactionLocation ) ;
	}

	private void fillAndDrawShape() {
		if ( this.smTactileEffect == SpaceMemoryTactileEffect.FALSE ) {
			this.g2d.setColor( this.smTactileEffect.getEffectColor() ) ;
			this.g2d.fill( this.smMove.getShape() ) ;
		} else {
			this.g2d.setColor( this.smLeftVisualEffect.getEffectColor() ) ;
			this.g2d.fill( this.smMove.getLeftHalfShape() ) ;
			this.g2d.setColor( this.smRightVisualEffect.getEffectColor() ) ;
			this.g2d.fill( this.smMove.getRightHalfShape() ) ;
		}

		this.g2d.setColor( Color.BLACK ) ;
		this.g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 20f ) ) ;
		// this.g2d.draw( this.smMove.getShape() ) ;
	}
}
