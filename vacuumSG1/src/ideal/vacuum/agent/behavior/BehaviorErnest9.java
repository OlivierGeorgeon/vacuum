package ideal.vacuum.agent.behavior ;

import java.awt.Color ;
import java.util.Map ;
import java.util.Map.Entry ;
import java.util.Queue ;

import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.DesignerListener ;
import ideal.vacuum.agent.GraphicProperties ;
import ideal.vacuum.agent.Move;
import ideal.vacuum.agent.TactileEffect ;
import ideal.vacuum.agent.VisualEffect ;
import ideal.vacuum.agent.vision.Eye ;
import ideal.vacuum.agent.vision.PhotoreceptorCell ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import eca.Primitive ;
import eca.PrimitiveImpl ;
import eca.spas.egomem.PlaceImpl ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class BehaviorErnest9 extends AbstractBehavior {

	public BehaviorErnest9( Ernest130Model model , DesignerListener listener , Eye eye ) {
		super( model , listener , eye ) ;
	}

	private void lookTheWorld() {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		this.colliculus.saccade();
		this.notifyBehaviorStateChanged( new BehaviorStateChangeEvent( this , this
				.getCurrentBehaviorState() ) ) ;
	}

	private void buildPlaces( String moveLabel, float angle , float xTranslation ) {
		this.places.clear();
		Map<PhotoreceptorCell , VisualEffect> stimuli = this.colliculus.visualEffect( angle , xTranslation ) ;
		for ( Entry<PhotoreceptorCell , VisualEffect> entry : stimuli.entrySet() ) {
			Primitive primitive = PrimitiveImpl.get( moveLabel + entry.getValue().getLabel() ) ;
			Point3f position = this.colliculus.getEventPosition( entry.getKey() , entry.getValue() ) ;
			PlaceImpl place = new PlaceImpl( primitive , position ) ;
			place.setValue(entry.getKey().getBlockColor().getRGB()); // OG
			this.places.add( place );
		}
	}
	
	private void addTactileOrVisualPlace( String label , Point3f location ) {
		Primitive primitive = PrimitiveImpl.get( label ) ;
		if (primitive != null){ // OG
			PlaceImpl place = new PlaceImpl( primitive , location ) ;
			this.places.add( place );
		}
		else 
			System.out.println("Illegal interaction label: " + label);
	}
	
	protected void turnRight() {
		this.turnRightAnimWorld() ;
		this.lookTheWorld() ;

		this.buildPlaces( Move.TURN_RIGHT.getLabel(), (float) Math.PI / 2 , 0 ) ;
		this.setTransform( (float) Math.PI / 2 , 0 );
	}

	protected void turnLeft() {
		this.turnLeftAnimWorld() ;
		this.lookTheWorld() ;

		this.buildPlaces( Move.TURN_LEFT.getLabel(), (float) -Math.PI / 2 , 0 ) ;
		this.setTransform( (float) -Math.PI / 2 , 0 );
	}

	protected void moveForward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;

		if ( this.model.getEnvironment().affordWalk( aheadPoint ) &&
				!this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnimWorld() ;
			this.lookTheWorld() ;
			this.buildPlaces( Move.MOVE_FORWARD.getLabel(), 0 , -1 );
			
			if ( this.model.getEnvironment().isFood( aheadPoint.x , aheadPoint.y ) ) {
				this.effect.setColor( this.model.getEnvironment()
						.seeBlock( aheadPoint.x , aheadPoint.y ).getRGB() ) ;
//				this.effect.setLocation( new Point3f() ) ;
				this.model.getEnvironment().eatFood( aheadPoint ) ;
//				this.effect.setLabel( TactileEffect.FOOD.getLabel() ) ;
				this.addTactileOrVisualPlace( Move.MOVE_FORWARD.getLabel() + TactileEffect.FOOD.getLabel() , new Point3f() ); // OG
				
			}
//			this.effect.setTransformation( 0 , -1 ) ;
			this.setTransform( 0 , -1 );

		} else {
			this.bumpAheadAnimWorld() ;
			this.lookTheWorld() ;
			this.buildPlaces( Move.MOVE_FORWARD.getLabel(), 0 , 0 );
			this.addTactileOrVisualPlace( Move.MOVE_FORWARD.getLabel() + TactileEffect.FALSE.getLabel() , new Point3f( 1 , 0 , 0 ) );
//			this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
//			this.effect.setColor( Color.RED.getRGB() );
//			this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
		}
	}

	protected void moveBackward() {
	}

	protected void touch() {
	}

	protected void touchLeft() {
	}

	protected void touchRight() {
	}
}
