package agent ;

import java.awt.Color ;

import javax.vecmath.Matrix3f ;
import javax.vecmath.Point3f ;
import javax.vecmath.Vector3f ;

import utils.ErnestUtils ;
import utils.Pair ;
import ernest.Effect ;
import ernest.Ernest ;
import ernest.IEffect ;

public class Behavior {

	public static Color UNANIMATED_COLOR = Color.GRAY ;
	private Color focusColor = UNANIMATED_COLOR ;
	private Color leftColor = UNANIMATED_COLOR ;
	private Color rightColor = UNANIMATED_COLOR ;
	private Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA] ;

	private Ernest130Model model ;
	private IEffect effect ;
	private Pair<Integer , Color>[] eyeFixation = null ;

	private float animOrientation = 0 ;
	private float animPosition = 0 ;
	private int delayMove = 2 ;
	private int delayTouch = 20 ;

	public Behavior( Ernest130Model model ) {
		this.model = model ;
		this.effect = new Effect() ;

		// Initialize Ernest's colors
		for ( int i = 0; i < Ernest.RESOLUTION_RETINA; i++ )
			this.pixelColor[i] = Behavior.UNANIMATED_COLOR ;

		this.focusColor = Behavior.UNANIMATED_COLOR ;
		this.leftColor = Behavior.UNANIMATED_COLOR ;
		this.rightColor = Behavior.UNANIMATED_COLOR ;
	}

	public BehaviorState getCurrentBehaviorState() {
		return new BehaviorState( this.animOrientation , this.animPosition , this.focusColor , this.leftColor , this.rightColor , this.pixelColor ) ;
	}

	public IEffect getEffect() {
		return this.effect ;
	}

	public BehaviorState doMovement( Schema schema ) {
		this.effect = new Effect() ;
		this.focusColor = Behavior.UNANIMATED_COLOR ;
		this.leftColor = Behavior.UNANIMATED_COLOR ;
		this.rightColor = Behavior.UNANIMATED_COLOR ;

		this.animOrientation = 0 ;
		this.animPosition = 0 ;

		System.out.println( "Agent #" + this.model.getID() + ", Step #" + this.model.getCounter() + "=======" ) ;

		switch ( schema ) {
			case MOVE:
				this.move() ;
				break ;
			case BACKWARD:
				this.backward() ;
				break ;
			case RIGHT:
				this.right() ;
				break ;
			case LEFT:
				this.left() ;
				break ;
			case TOUCH:
				this.touch() ;
				break ;
			case TOUCH_LEFT:
				this.touchLeft() ;
				break ;
			case TOUCH_RIGHT:
				this.touchRight() ;
				break ;
			default:
				break ;
		}

		this.eyeFixation = this.model.getRetina( this.model.getmOrientation().z ) ;

		for ( int i = 0; i < Ernest.RESOLUTION_RETINA; i++ )
			this.pixelColor[i] = this.eyeFixation[i].mRight ;

		// Trace the environmental data
		if ( this.model.tracer != null ) {
			Object e = this.model.tracer.addEventElement( "environment" ) ;
			this.model.tracer.addSubelement( e , "x" , ErnestUtils.format( this.model.getmPosition().x , 0 ) ) ;
			this.model.tracer.addSubelement( e , "y" , ErnestUtils.format( this.model.getmPosition().y , 0 ) ) ;
			this.model.tracer.addSubelement( e , "orientation" , ErnestUtils.format( this.model.getmOrientation().z , 2 ) ) ;
		}

		return new BehaviorState( this.animOrientation , this.animPosition , this.focusColor , this.leftColor , this.rightColor , this.pixelColor ) ;
	}

	private void right() {
		for ( int i = 0; i < 20; i++ ) {
			this.model.getmOrientation().z -= Math.PI / 40 ;
			this.animOrientation -= Math.PI / 40 ;
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
		if ( this.model.getmOrientation().z < -Math.PI )
			this.model.getmOrientation().z += 2 * Math.PI ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) Math.PI / 2 , 0 ) ;
	}

	private void left() {
		for ( int i = 0; i < 20; i++ ) {
			this.model.getmOrientation().z += Math.PI / 40 ;
			this.animOrientation += Math.PI / 40 ;
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
		if ( this.model.getmOrientation().z > Math.PI )
			this.model.getmOrientation().z -= 2 * Math.PI ;
		this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		this.effect.setTransformation( (float) -Math.PI / 2 , 0 ) ;
	}

	private void move() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;
		
		if ( this.model.getEnvironment().affordWalk( point ) && ! this.model.affordCuddle( point ) ) {
			for ( int i = 0; i < 20; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
				this.animPosition += .05 ;
				this.anim() ;
				this.model.sleep( this.delayMove ) ;
			}
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setTransformation( 0 , -1 ) ;
		}
		else
		{
			this.effect.setColor( 0xFF0000 ) ;
			this.focusColor = Color.RED ;
			for ( int i = 0; i < 5; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
				this.anim() ;
				this.model.sleep( 20 ) ;
			}
			for ( int i = 0; i < 5; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
				this.anim() ;
				this.model.sleep( 20 ) ;
			}
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		}
	}

	private void backward() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_BEHIND ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		this.effect.setLocation( new Point3f( -1 , 0 , 0 ) ) ;
		
		if ( this.model.getEnvironment().affordWalk( point ) && ! this.model.affordCuddle( point ) ) {
			for ( int i = 0; i < 20; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
				this.animPosition -= .05 ;
				this.anim() ;
				this.model.sleep( this.delayMove ) ;
			}
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			this.effect.setTransformation( 0 , 1 ) ;
		}
		else {
			this.focusColor = Color.RED ;
			for ( int i = 0; i < 5; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
				this.anim() ;
				this.model.sleep( 20 ) ;
			}
			for ( int i = 0; i < 5; i++ ) {
				this.model.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
				this.anim() ;
				this.model.sleep( 20 ) ;
			}
			// status = FEEDBACK_FALSE;
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		}
	}

	private void touch() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_AHEAD ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.focusColor = blockColor ;
		this.effect.setLocation( new Point3f( 1 , 0 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;
		
		if( this.model.affordCuddle( point ) ){
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
//			ErnestModel entity = this.model.getEnvironment().getEntity( point , this.model.getName() ) ;
			this.focusColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		}
		else if ( this.model.getEnvironment().affordWalk( point ) ) {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		}
		else {
			if ( blockColor.equals( Environment.WALL1 ) )
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
			else
				this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		}
		this.anim() ;
		this.model.sleep( this.delayTouch ) ;
	}

	private void touchLeft() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_LEFT ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.leftColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , 1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;
		
		if( this.model.affordCuddle( point ) ){
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
//			ErnestModel entity = this.model.getEnvironment().getEntity( point , this.model.getName() ) ;
			this.leftColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		}
		else if ( this.model.getEnvironment().affordWalk( point ) ) {
				this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		}
		else
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		this.anim() ;
		this.model.sleep( this.delayTouch ) ;
	}

	private void touchRight() {
		Vector3f localPoint = new Vector3f( this.model.DIRECTION_RIGHT ) ;
		Vector3f point = this.model.localToParentRef( localPoint ) ;
		
		Color blockColor = this.model.getEnvironment().seeBlock( point.x , point.y ) ;
		this.rightColor = blockColor ;
		this.effect.setLocation( new Point3f( 0 , -1 , 0 ) ) ;
		this.effect.setColor( blockColor.getRGB() ) ;
		
		if( this.model.affordCuddle( point ) ){
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
//			ErnestModel entity = this.model.getEnvironment().getEntity( point , this.model.getName() ) ;
			this.rightColor = Environment.WALL1 ;
			this.effect.setColor( Environment.WALL1.getRGB() ) ;
		}
		else if ( this.model.getEnvironment().affordWalk( point ) ) {
			this.effect.setLabel( Stimuli.FALSE.getLabel() ) ;
		}
		else
			this.effect.setLabel( Stimuli.TRUE.getLabel() ) ;
		this.anim() ;
		this.model.sleep( delayTouch ) ;
	}

	public void anim() {
		// compute absolute movements
		this.model.mSpeedT = new Vector3f( this.model.getmPosition() ) ;
		this.model.mSpeedT.sub( this.model.getmPreviousPosition() ) ;

		this.model.mSpeedR = new Vector3f( this.model.getmOrientation() ) ;
		this.model.mSpeedR.sub( this.model.getmPreviousOrientation() ) ;

		Matrix3f rot2 = new Matrix3f() ;
		rot2.rotZ( -this.model.getmOrientation().z ) ;
		rot2.transform( this.model.mSpeedT , this.model.mEgoSpeedT ) ;

		if ( this.model.mSpeedR.z > Math.PI )
			this.model.mSpeedR.z -= 2 * Math.PI ;
		if ( this.model.mSpeedR.z <= -Math.PI )
			this.model.mSpeedR.z += 2 * Math.PI ;

		this.model.getMainFrame().drawGrid() ;

		for ( int i = 0; i < this.model.getEnvironment().frameList.size(); i++ ) {
			this.model.getEnvironment().frameList.get( i ).repaint() ;
		}
	}
}
