package ideal.vacuum.agent.behavior ;

import ideal.vacuum.Environment ;
import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.DesignerListener ;
import ideal.vacuum.agent.TactileEffect ;

import java.awt.Color ;

import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class BehaviorErnest7 extends AbstractBehavior {

	public BehaviorErnest7( Ernest130Model model , DesignerListener listener ) {
		super( model , listener ) ;
	}

	protected void turnRight() {
		this.turnRightAnimWorld() ;
		this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	protected void turnLeft() {
		this.turnLeftAnimWorld() ;
		this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	protected void moveForward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f aheadPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( aheadPoint.x , aheadPoint.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.getEnvironment().affordWalk( aheadPoint ) && !this.model.affordCuddle( aheadPoint ) ) {
			this.moveForwardAnimWorld() ;
			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
			this.effect.setTransformation( 0 , -1 ) ;
		} else {
			this.effect.setColor( Color.RED.getRGB() ) ;
			this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
			this.focusColor = Color.RED ;
			this.bumpAheadAnimWorld() ;
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
			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
			this.focusColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( aheadPoint ) ) {
			this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
			this.focusColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.focusColor = Environment.WALL1 ;
			} else {
				this.focusColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
		}
		this.touchAnimWorld() ;
	}

	protected void touchLeft() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_LEFT ) ;
		Vector3f leftPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( leftPoint.x , leftPoint.y ) ;
		this.leftColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , 1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( leftPoint ) ) {
			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
			this.leftColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( leftPoint ) ) {
			this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
			this.leftColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.leftColor = Environment.WALL1 ;
			} else {
				this.leftColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
		}

		this.touchAnimWorld() ;
	}

	protected void touchRight() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_RIGHT ) ;
		Vector3f rightPoint = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( rightPoint.x , rightPoint.y ) ;
		this.rightColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , -1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;

		if ( this.model.affordCuddle( rightPoint ) ) {
			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
			this.rightColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		} else if ( this.model.getEnvironment().affordWalk( rightPoint ) ) {
			this.effect.setLabel( TactileEffect.FALSE.getLabel() ) ;
			this.rightColor = Environment.FIELD_COLOR ;
		} else {
			if ( blockColor.equals( Environment.WALL1 ) ||
					blockColor.equals( Environment.WALL2 ) ||
					blockColor.equals( Environment.WALL3 ) ) {
				this.rightColor = Environment.WALL1 ;
			} else {
				this.rightColor = Environment.FIELD_COLOR ;
			}

			this.effect.setLabel( TactileEffect.TRUE.getLabel() ) ;
		}

		this.touchAnimWorld() ;
	}
}
