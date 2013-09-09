package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.behavior.BehaviorState ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f;
import eca.spas.Placeable;
import eca.spas.egomem.SpatialMemoryImpl;


/**
 * @author Olivier
 */
public class PhenomenonInstanceDesigner extends AbstractSMInteractionDesigner {

	private Graphics2D g2d ;
	private SpaceMemoryMove smMove ;
	//private SpaceMemoryTactileEffect smTactileEffect ;
	//private SpaceMemoryVisualEffect smVisualEffect ;

	@Override
	public void addInteraction( Graphics2D g2d , Placeable placeable , BehaviorState behaviorState ) {
		this.g2d = g2d ;
		double scale = (double)(SpatialMemoryImpl.PERSISTENCE_DURATION - placeable.getClock() ) / SpatialMemoryImpl.PERSISTENCE_DURATION * 3d;
		this.applyGeometricalTransformation(
				placeable.getOrientationAngle() , 
				placeable.getPosition() ,
				scale ) ;

		this.smMove = SpaceMemoryMove.getSpaceMemoryMove( "" ) ;
		
		this.fillAndDrawPhenomenon(placeable.getDisplayCode());
		
//		if ( SpaceMemoryTactileEffect.containTactileEffect( interactionLabel ) ) {
//			String tactileEffectLabel = SpaceMemoryTactileEffect.extractTactileEffectLabel( interactionLabel ) ;
//			this.smTactileEffect = SpaceMemoryTactileEffect.getSpaceMemoryTactileEffect( tactileEffectLabel ) ;
//		}
//
//		if ( SpaceMemoryVisualEffect.containVisualEffect( interactionLabel ) ) {
//			String visualEffectLabel = SpaceMemoryVisualEffect.extractLeftVisualEffectLabel( interactionLabel ) ;
//			this.smVisualEffect = SpaceMemoryVisualEffect.getSpaceMemoryVisualEffect( visualEffectLabel ) ;			
//			this.fillAndDrawShapeVisual(placeable.getClock()) ;
//		}else{
//			this.fillAndDrawShapeNoVisual();
//		}
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

	private void fillAndDrawPhenomenon(int displayCode) {
		this.g2d.setColor( new Color(displayCode) ) ;
		this.g2d.fill( this.smMove.getShape() ) ;
		this.g2d.setColor( Color.BLACK ) ;
		this.g2d.setStroke( new BasicStroke( SpaceMemoryDesigner.SCALE / 100f ) ) ;
		this.g2d.draw( this.smMove.getShape() ) ;
	}
}
