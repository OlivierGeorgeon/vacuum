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
	private SpaceMemoryVisualEffect smVisualEffect ;

	@Override
	public void addInteraction( Graphics2D g2d , IPlace place , BehaviorState behaviorState ) {
		this.g2d = g2d ;
		this.applyGeometricalTransformation(
				place.getOrientationAngle() ,
				place.getPosition() ,
				( LocalSpaceMemory.PERSISTENCE_DURATION - place.getClock() ) / 1.5f ) ;

		String interactionLabel = place.getInteraction().getLabel() ;
		this.smMove = SpaceMemoryMove.getSpaceMemoryMove( SpaceMemoryMove
				.extractMoveLabel( interactionLabel ) ) ;

		if ( SpaceMemoryVisualEffect.containVisualEffect( interactionLabel ) ) {
			String visualEffectLabel = SpaceMemoryVisualEffect.extractLeftVisualEffectLabel( interactionLabel ) ;
			this.smVisualEffect = SpaceMemoryVisualEffect.getSpaceMemoryVisualEffect( visualEffectLabel ) ;			
			this.fillAndDrawShapeVisual() ;
		}
		else {
			this.smTactileEffect = SpaceMemoryTactileEffect
			.getSpaceMemoryTactileEffect( SpaceMemoryTactileEffect
					.extractTactileEffectLabel( interactionLabel ) ) ;
			this.fillAndDrawShapeNoVisual();
		}
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

	private void fillAndDrawShapeVisual() {
		this.g2d.setColor( this.smVisualEffect.getEffectColor() ) ;
		this.g2d.fill( this.smMove.getShape() ) ;
		this.g2d.setColor( Color.BLACK ) ;
		this.g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 20f ) ) ;
	}
	private void fillAndDrawShapeNoVisual() {
		//this.g2d.setColor( Color.RED ) ;
		this.g2d.setColor( this.smTactileEffect.getEffectColor() ) ;
		this.g2d.fill( this.smMove.getShape() ) ;
	}
}
