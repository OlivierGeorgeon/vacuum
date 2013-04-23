package ideal.vacuum.agent.behavior ;

import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.GraphicProperties ;
import ideal.vacuum.agent.GraphicPropertiesListener ;
import ideal.vacuum.agent.TactileEffect ;
import ideal.vacuum.agent.VisualEffect ;
import ideal.vacuum.agent.vision.Eyes ;
import ideal.vacuum.agent.vision.PhotoreceptorCell ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

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
		PhotoreceptorCell[] retina = this.model.getRetina( ernestGraphicProperties
				.getmOrientation().z ) ;
		this.eyes.updateRightEye(
				retina[0].getxBlockPosition() ,
				retina[0].getyBlockPosition() ,
				retina[0].getBlockColor() ) ;
		this.eyes.updateLeftEye(
				retina[1].getxBlockPosition() ,
				retina[1].getyBlockPosition() ,
				retina[1].getBlockColor() ) ;
	}

	private String getEyesStimuli( Eyes previousSnapshot , Eyes currentSnapshot ) {
		String eyesStimuli = this.determineVisualEffect(
				(int) previousSnapshot.getLeftEyeDistanceAccurateToTheblock() ,
				(int) currentSnapshot.getLeftEyeDistanceAccurateToTheblock() ).getLabel() ;
		eyesStimuli += this.determineVisualEffect(
				(int) previousSnapshot.getRightEyeDistanceAccurateToTheblock() ,
				(int) currentSnapshot.getRightEyeDistanceAccurateToTheblock() ).getLabel() ;

		return eyesStimuli ;
	}

	private VisualEffect determineVisualEffect( int previousDistance , int currentDistance ) {
		VisualEffect stimuli = VisualEffect.UNCHANGED ;

		if ( previousDistance == currentDistance ) {
			stimuli = VisualEffect.UNCHANGED ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance ) {
			stimuli = VisualEffect.CLOSER ;
		} else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE ) {
			stimuli = VisualEffect.APPEAR ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE ) {
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
		float d = Ernest.INT_FACTOR ;
		switch ( this.eyes.getActifEye() ) {
			case LEFT:
				d = (float) this.eyes.getLeftEyeDistanceToTheblock() ;
				if ( d > 0 && Math.abs( d ) < Ernest.INFINITE ) // d=1;
					this.effect.setLocation( new Point3f( this.eyes.getLeftxToTheBlock() /
							d *
							Eyes.DISTANCE_VISION , this.eyes.getLeftxToTheBlock() /
							d *
							Eyes.DISTANCE_VISION , 0 ) ) ;
				else
					this.effect.setLocation( new Point3f( 0 , Eyes.DISTANCE_VISION , 0 ) ) ;
				break ;
			case RIGHT:
				d = (float) this.eyes.getRightEyeDistanceToTheblock() ;
				if ( d > 0 ) // d=1;
					this.effect.setLocation( new Point3f( this.eyes.getRightxToTheBlock() /
							d *
							Eyes.DISTANCE_VISION , this.eyes.getRightyToTheBlock() /
							d *
							Eyes.DISTANCE_VISION , 0 ) ) ;
				else
					this.effect.setLocation( new Point3f( 0 , -Eyes.DISTANCE_VISION , 0 ) ) ;
				break ;
			case BOTH:
				d = (float) this.eyes.getLeftEyeDistanceToTheblock() ;
				if ( d > 0 )
					this.effect.setLocation( new Point3f( this.eyes.getLeftxToTheBlock() /
							d *
							Eyes.DISTANCE_VISION , 0 , 0 ) ) ;
				else
					this.effect.setLocation( new Point3f( Eyes.DISTANCE_VISION , 0 , 0 ) ) ;
				break ;
			case NONE:
				this.effect.setLocation( new Point3f( 0 , 0 , 0 ) ) ;
				break ;
			default:
				break ;
		}
	}

	protected void turnRight() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnRightAnim() ;
		this.lookTheWorld() ;

		String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) +
				TactileEffect.TRUE.getLabel() ;
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
		this.setLocationFromEyes() ;
	}

	protected void turnLeft() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnLeftAnim() ;
		this.lookTheWorld() ;

		String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) +
				TactileEffect.TRUE.getLabel() ;
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
		this.setLocationFromEyes() ;
	}

	protected void moveForward() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;

		if ( this.model.getEnvironment().affordWalk( aheadPoint ) &&
				!this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnim() ;
			this.lookTheWorld() ;
			this.setLocationFromEyes() ;
			if ( this.model.getEnvironment().isFood( aheadPoint.x , aheadPoint.y ) ) {
				this.effect.setColor( this.model.getEnvironment()
						.seeBlock( aheadPoint.x , aheadPoint.y ).getRGB() ) ;
				this.effect.setLocation( new Point3f() ) ;
				this.model.getEnvironment().eatFood( aheadPoint ) ;
				this.effect.setLabel( TactileEffect.FOOD.getLabel() ) ;
			} else {
				String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) +
						TactileEffect.TRUE.getLabel() ;
				this.effect.setLabel( tactileStimuli ) ;
			}
			this.effect.setTransformation( 0 , -1 ) ;

		} else {
			this.bumpAheadAnim() ;
			this.lookTheWorld() ;
			this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
			String tactileStimuli = this.getEyesStimuli( snapshot , this.eyes ) +
					TactileEffect.FALSE.getLabel() ;
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
