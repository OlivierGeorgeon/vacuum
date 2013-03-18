package agent.model ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import agent.Environment ;
import agent.Ernest130Model ;

public class BehaviorErnest7 extends AbstractBehavior {

	public BehaviorErnest7( Ernest130Model model , GraphicPropertiesListener listener ) {
		super( model , listener ) ;
	}

	protected void turnRight() {
		this.turnRightAnim() ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	protected void turnLeft() {
		this.turnLeftAnim() ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	protected void moveForward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( aheadPoint.x , aheadPoint.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.getEnvironment().affordWalk( aheadPoint ) && !this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnim() ;
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setTransformation( 0 , -1 ) ;
		} else {
			this.effect.setColor( Color.RED.getRGB() ) ;
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.focusColor = Color.RED ;
			this.bumpAheadAnim() ;
		}
	}

	protected void moveBackward() {
	}

	protected void touch() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( aheadPoint.x , aheadPoint.y ) ;
		this.focusColor = blockColor ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( aheadPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.focusColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( aheadPoint ) ) {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.focusColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.focusColor = Environment.WALL1 ;
			} else {
				this.focusColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		}
		this.touchAnim() ;
	}

	protected void touchLeft() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_LEFT ) ;
		Vector3f leftPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( leftPoint.x , leftPoint.y ) ;
		this.leftColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , 1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( leftPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.leftColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( leftPoint ) ) {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.leftColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.leftColor = Environment.WALL1 ;
			} else {
				this.leftColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		}

		this.touchAnim() ;
	}

	protected void touchRight() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_RIGHT ) ;
		Vector3f rightPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( rightPoint.x , rightPoint.y ) ;
		this.rightColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , -1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( rightPoint ) ) {
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			// ErnestModel entity = this.model.getEnvironment().getEntity( point
			// , this.model.getName() ) ;
			this.rightColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( rightPoint ) ) {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
			this.rightColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.rightColor = Environment.WALL1 ;
			} else {
				this.rightColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		}

		this.touchAnim() ;
	}
}
