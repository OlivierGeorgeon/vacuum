package agent.model.behavior ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import utils.Pair ;
import agent.Ernest130Model ;
import agent.model.GraphicProperties ;
import agent.model.GraphicPropertiesListener ;
import agent.model.TactileEffect ;
import agent.model.VisualEffect ;
import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class BehaviorErnest9 extends AbstractBehavior {

	public BehaviorErnest9( Ernest130Model model , GraphicPropertiesListener listener ) {
		super( model , listener ) ;
	}

	private void lookTheWorld() {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		Pair<Integer , Color>[] retina = this.model.getRetina( ernestGraphicProperties
				.getmOrientation().z ) ;
		this.eyes.updateRightEye( retina[0].getRight() , retina[0].getLeft() ) ;
		this.eyes.updateLeftEye( retina[1].getRight() , retina[1].getLeft() ) ;
	}
	
	private String getEyesStimuli( Eyes previousSnapshot , Eyes currentSnapshot ) {
		String eyesStimuli = this.determineVisualEffect( previousSnapshot.getLeftEyeDistanceToTheblock() , currentSnapshot.getLeftEyeDistanceToTheblock() ).getLabel();
		eyesStimuli += this.determineVisualEffect( previousSnapshot.getRightEyeDistanceToTheblock() , currentSnapshot.getRightEyeDistanceToTheblock() ).getLabel();
		
		return eyesStimuli;
	}

	private VisualEffect determineVisualEffect( int previousDistance , int currentDistance ){
		VisualEffect stimuli = VisualEffect.UNCHANGED ;
		
		if ( previousDistance == currentDistance ) {
			stimuli = VisualEffect.UNCHANGED ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance  ) {
			stimuli = VisualEffect.CLOSER ;
		}else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE   ) {
			stimuli = VisualEffect.APPEAR ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE   ) {
			stimuli = VisualEffect.DISAPPEAR ;
		}

		System.out.println( "Sensed " +
				"prev=" +
				previousDistance +
				" cur=" +
				currentDistance +
				" stimuli " +
				stimuli.getLabel() ) ;

		return stimuli ;
	}
	
	private void setLocationFromEyes() {
		switch ( this.eyes.getActifEye() ) {
			case LEFT:
				this.effect.setLocation( new Point3f( 4 , 4 , 0 ) );
				break ;
			case RIGHT:
				this.effect.setLocation( new Point3f( 4 , -4 , 0 ) );
				break;
			case BOTH:
				this.effect.setLocation( new Point3f( 4 , 0 , 0 ) );
				break;
			case NONE:
				this.effect.setLocation( new Point3f( 0 , 0 , 0 ) );
				break;
			default:
				break ;
		}
	}
	
	protected void turnRight() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnRightAnim() ;
		this.lookTheWorld() ;
		
		String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) + TactileEffect.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
		this.setLocationFromEyes() ;
	}

	protected void turnLeft() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnLeftAnim() ;
		this.lookTheWorld() ;
		
		String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) + TactileEffect.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
		this.setLocationFromEyes() ;
	}

	protected void moveForward() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		
		if ( this.model.getEnvironment().affordWalk( aheadPoint ) && !this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnim() ;
			this.lookTheWorld() ;
			this.setLocationFromEyes() ;
			if ( this.model.getEnvironment().isFood( aheadPoint.x , aheadPoint.y ) ) {
				this.model.getEnvironment().eatFood( aheadPoint );
				String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) + TactileEffect.TRUE.getLabel();
				this.effect.setLabel( tactileStimuli ) ;
			} else {
				String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) + TactileEffect.TRUE.getLabel();
				this.effect.setLabel( tactileStimuli ) ;
			}
			this.effect.setTransformation( 0 , -1 ) ;
			
		} else {
			this.bumpAheadAnim() ;
			this.lookTheWorld() ;
			this.effect.setLocation( new Point3f( 1 , 0 , 0 ) );
			String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) + TactileEffect.FALSE.getLabel();
			this.effect.setLabel( tactileStimuli ) ;
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
