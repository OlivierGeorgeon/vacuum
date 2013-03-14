package agent.model ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import agent.Environment ;
import agent.Ernest130Model ;
import agent.ErnestModel ;

public class BehaviorErnest8 extends AbstractBehavior {

	public BehaviorErnest8( Ernest130Model model , GraphicPropertiesListener listener ) {
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
			this.focusColor = Color.RED ;
			this.bumpAheadAnim() ;
		}
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
			ErnestModel entity = this.model.getEnvironment().getEntity(
					aheadPoint ,
					this.model.getName() ) ;
			this.focusColor = entity.getColor() ;
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
			ErnestModel entity = this.model.getEnvironment().getEntity(
					leftPoint ,
					this.model.getName() ) ;
			this.leftColor = entity.getColor() ;
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
			ErnestModel entity = this.model.getEnvironment().getEntity(
					rightPoint ,
					this.model.getName() ) ;
			this.rightColor = entity.getColor() ;
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
	}
}