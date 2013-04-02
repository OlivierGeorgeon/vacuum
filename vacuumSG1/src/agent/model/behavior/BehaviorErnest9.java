package agent.model.behavior ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import utils.Pair ;
import agent.Ernest130Model ;
import agent.model.GraphicProperties ;
import agent.model.GraphicPropertiesListener ;
import agent.model.TactileStimuli ;
import agent.model.VisualStimuli ;
import ernest.Ernest ;

public class BehaviorErnest9 extends AbstractBehavior {

	public BehaviorErnest9( Ernest130Model model , GraphicPropertiesListener listener ) {
		super( model , listener ) ;
	}

	private void seeTheWorld() {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		Pair<Integer , Color>[] retina = this.model.getRetina( ernestGraphicProperties
				.getmOrientation().z ) ;
		this.eyes.updateRightEye( retina[0].getRight() , retina[0].getLeft() ) ;
		this.eyes.updateLeftEye( retina[1].getRight() , retina[1].getLeft() ) ;
	}

	private VisualStimuli oneEyeStimuli( int previousDistance , int currentDistance ){
		VisualStimuli stimuli = VisualStimuli.UNCHANGED ;
		
		if ( previousDistance == currentDistance ) {
			stimuli = VisualStimuli.UNCHANGED ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance  ) {
			stimuli = VisualStimuli.CLOSER ;
		}else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE   ) {
			stimuli = VisualStimuli.APPEAR ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE   ) {
			stimuli = VisualStimuli.DISAPPEAR ;
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
	
	private String allEyesStimuli( Eyes previousSnapshot , Eyes currentSnapshot ) {
		String eyesStimuli = this.oneEyeStimuli( previousSnapshot.getLeftEyeDistanceToTheblock() , currentSnapshot.getLeftEyeDistanceToTheblock() ).getLabel();
		eyesStimuli += this.oneEyeStimuli( previousSnapshot.getRightEyeDistanceToTheblock() , currentSnapshot.getRightEyeDistanceToTheblock() ).getLabel();
		
		return eyesStimuli;
	}

	protected void turnRight() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnRightAnim() ;
		this.seeTheWorld() ;
		
		String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + TactileStimuli.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	protected void turnLeft() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnLeftAnim() ;
		this.seeTheWorld() ;
		
		String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + TactileStimuli.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	protected void moveForward() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( aheadPoint.x , aheadPoint.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;

		if ( this.model.getEnvironment().affordWalk( aheadPoint ) && !this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnim() ;
			this.seeTheWorld() ;
			if ( this.model.getEnvironment().isFood( aheadPoint.x , aheadPoint.y ) ) {
				this.model.getEnvironment().eatFood( aheadPoint );
				this.effect.setLabel( TactileStimuli.FOOD.getLabel() ) ;
				this.effect.setColor( blockColor.getRGB() );
			} else {
				String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + TactileStimuli.TRUE.getLabel();
				this.effect.setLabel( tactileStimuli ) ;
			}
			this.effect.setTransformation( 0 , -1 ) ;
		} else {
			this.bumpAheadAnim() ;
			this.seeTheWorld() ;
			String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + TactileStimuli.FALSE.getLabel();
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
