package agent.model ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import utils.Pair ;
import agent.Environment ;
import agent.Ernest130Model ;

public class BehaviorErnest9 extends AbstractBehavior {

	public BehaviorErnest9( Ernest130Model model , GraphicPropertiesListener listener ) {
		super( model , listener ) ;
	}

	private void seeTheWorld() {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		Pair<Integer , Color>[] retina = this.model.getRetina( ernestGraphicProperties.getmOrientation().z ) ;
		this.rightEyeColor = retina[0].getRight();
		this.leftEyeColor = retina[1].getRight();
	}
	
	protected void turnRight() {		
		this.turnRightAnim() ;
		this.seeTheWorld() ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	protected void turnLeft() {
		this.turnLeftAnim() ;
		this.seeTheWorld() ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	protected void moveForward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.getEnvironment().affordWalk( point ) && !this.model.affordCuddle( point ) ) {
			this.moveForwardAnim() ;
			if ( blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA2 ) ||
					blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA3 ) ||
					blockColor.equals( Environment.ALGA4 ) ||
					blockColor.equals( Environment.ALGA5 ) ) {
				this.effect.setLabel( Stimuli.ALGA.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			}
			this.effect.setTransformation( 0 , -1 ) ;
		} else {
			this.effect.setColor( Color.RED.getRGB() ) ;
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.bumpAheadAnim() ;
		}
		this.seeTheWorld() ;
	}

	protected void moveBackward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_BEHIND ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		this.effect.setLocation( new Point3f( -1 , 0 , 0 ) ) ;

		if ( this.model.getEnvironment().affordWalk( point ) && !this.model.affordCuddle( point ) ) {
			this.moveBackwardAnim() ;
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setTransformation( 0 , 1 ) ;
		} else {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.effect.setColor( Color.RED.getRGB() ) ;
			this.focusColor = Color.RED ;
			this.bumpBehindAnim() ;
		}
		this.seeTheWorld() ;
	}

	protected void touch() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( aheadPoint.x , aheadPoint.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( aheadPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setColor( Environment.AGENT.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( aheadPoint ) ) {
			if ( blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA2 ) ||
					blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA3 ) ||
					blockColor.equals( Environment.ALGA4 ) ||
					blockColor.equals( Environment.ALGA5 ) ) {
				this.effect.setLabel( Stimuli.ALGA.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			}
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.BRICK.getLabel() ) ;
			}
		}
		this.touchAnim() ;
		this.seeTheWorld() ;
	}

	protected void touchLeft() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_LEFT ) ;
		Vector3f leftPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( leftPoint.x , leftPoint.y ) ;
		this.effect.setLocation( new Point3f( 0 , 1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( leftPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setColor( Environment.AGENT.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( leftPoint ) ) {
			if ( blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA2 ) ||
					blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA3 ) ||
					blockColor.equals( Environment.ALGA4 ) ||
					blockColor.equals( Environment.ALGA5 ) ) {
				this.effect.setLabel( Stimuli.ALGA.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			}
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.BRICK.getLabel() ) ;
			}
		}

		this.touchAnim() ;
		this.seeTheWorld() ;
	}

	protected void touchRight() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_RIGHT ) ;
		Vector3f rightPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( rightPoint.x , rightPoint.y ) ;
		this.effect.setLocation( new Point3f( 0 , -1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( rightPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setColor( Environment.AGENT.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( rightPoint ) ) {
			if ( blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA2 ) ||
					blockColor.equals( Environment.ALGA1 ) ||
					blockColor.equals( Environment.ALGA3 ) ||
					blockColor.equals( Environment.ALGA4 ) ||
					blockColor.equals( Environment.ALGA5 ) ) {
				this.effect.setLabel( Stimuli.ALGA.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			}
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			} else {
				this.effect.setLabel( Stimuli.BRICK.getLabel() ) ;
			}
		}

		this.touchAnim() ;
		this.seeTheWorld() ;
	}
}
