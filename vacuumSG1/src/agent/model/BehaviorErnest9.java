package agent.model ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import ernest.Ernest ;

import utils.Pair ;
import agent.Block ;
import agent.Environment ;
import agent.Ernest130Model ;

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

	private Stimuli oneEyeStimuli( int previousDistance , int currentDistance , Color colorSeen ){
		Stimuli stimuli = Stimuli.UNCHANGED ;
		
		if ( previousDistance == currentDistance ) {
			stimuli = Stimuli.UNCHANGED ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance  ) {
			stimuli = Stimuli.CLOSER ;
		}else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE   ) {
			stimuli = Stimuli.APPEAR ;
		}else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE   ) {
			stimuli = Stimuli.DISAPPEAR ;
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
		String eyesStimuli = this.oneEyeStimuli( previousSnapshot.getLeftEyeDistanceToTheblock() , currentSnapshot.getLeftEyeDistanceToTheblock() , currentSnapshot.getLeftEyeLookedBlock() ).getLabel();
		eyesStimuli += this.oneEyeStimuli( previousSnapshot.getRightEyeDistanceToTheblock() , currentSnapshot.getRightEyeDistanceToTheblock() , currentSnapshot.getRightEyeLookedBlock() ).getLabel();
		
		return eyesStimuli;
	}

	protected void turnRight() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnRightAnim() ;
		this.seeTheWorld() ;
		
		String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + Stimuli.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	protected void turnLeft() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		this.turnLeftAnim() ;
		this.seeTheWorld() ;
		
		String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + Stimuli.TRUE.getLabel();
		this.effect.setLabel( tactileStimuli ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	protected void moveForward() {
		Eyes snapshot = this.eyes.takeSnapshot() ;
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;

		if ( this.model.getEnvironment().affordWalk( point ) && !this.model.affordCuddle( point ) ) {
			this.moveForwardAnim() ;
			this.seeTheWorld() ;
			if ( this.model.getEnvironment().isFood( point.x , point.y ) ) {
				GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
				this.model.getEnvironment().eatFood( point );
				
				this.effect.setLabel( Stimuli.ALGA.getLabel() ) ;
			} else {
				String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + Stimuli.TRUE.getLabel();
				this.effect.setLabel( tactileStimuli ) ;
			}
			this.effect.setTransformation( 0 , -1 ) ;
		} else {
			this.bumpAheadAnim() ;
			this.seeTheWorld() ;
			String tactileStimuli = this.allEyesStimuli( snapshot , this.eyes ) + Stimuli.FALSE.getLabel();
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
