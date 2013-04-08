package agent.model.behavior;

import java.awt.Color ;

import javax.swing.event.EventListenerList ;
import javax.vecmath.Matrix3f ;
import javax.vecmath.Vector3f ;

import agent.Ernest130Model ;
import agent.model.AgentDesigner ;
import agent.model.GraphicProperties ;
import agent.model.GraphicPropertiesChangeEvent ;
import agent.model.GraphicPropertiesListener ;
import agent.model.Move ;
import ernest.Effect ;
import ernest.IEffect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public abstract class AbstractBehavior implements Behavior {

	protected int delayMove = 40 ; // 2
	protected int delayTouch = 50 ; // 50
	
	protected Color focusColor;
	protected Color leftColor;
	protected Color rightColor;
	protected Eyes eyes;
	
	protected Ernest130Model model ;
	protected IEffect effect ;
	
	protected EventListenerList listeners;
	
	public AbstractBehavior( Ernest130Model model , GraphicPropertiesListener listener ) {
		this.model = model ;
		this.effect = new Effect() ;
		this.listeners = new EventListenerList();
		this.listeners.add( GraphicPropertiesListener .class, listener);
		
		this.focusColor = AgentDesigner.UNANIMATED_COLOR ;
		this.leftColor = AgentDesigner.UNANIMATED_COLOR ;
		this.rightColor = AgentDesigner.UNANIMATED_COLOR ;
		this.eyes = new Eyes();
	}
	
	@Override
	public final BehaviorState getCurrentBehaviorState() {
		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.eyes ) ;
	}

	@Override
	public final IEffect getEffect() {
		return this.effect ;
	}
	
	protected final void notifyGraphicPropertiesChange( GraphicPropertiesChangeEvent event ){
		for ( GraphicPropertiesListener listener : this.listeners.getListeners( GraphicPropertiesListener.class ) ) {
			listener.notifyGraphicPropertiesChanged( event );
		}
	}
	
	public BehaviorState doMovement( Move schema ) {
		this.effect = new Effect() ;
		this.focusColor = AgentDesigner.UNANIMATED_COLOR ;
		this.leftColor = AgentDesigner.UNANIMATED_COLOR ;
		this.rightColor = AgentDesigner.UNANIMATED_COLOR ;
	
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
				this ,
				ernestGraphicProperties ) ;
		event.setAnimOrientation( 0 ) ;
		event.setAnimPosition( 0 ) ;
		this.notifyGraphicPropertiesChange( event ) ;
	
		System.out.println( "Agent #" +
				this.model.getID() +
				", Step #" +
				this.model.getCounter() +
				"=======" ) ;
	
		switch ( schema ) {
			case MOVE_FORWARD:
				this.moveForward() ;
				break ;
			case MOVE_BACKWARD:
				this.moveBackward() ;
				break ;
			case TURN_RIGHT:
				this.turnRight() ;
				break ;
			case TURN_LEFT:
				this.turnLeft() ;
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
	
		return new BehaviorState(
				this.focusColor ,
				this.leftColor ,
				this.rightColor ,
				this.eyes ) ;
	}
	
	@Override
	public final void anim() {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		this.computeAbsoluteSpeedTranslation( ernestGraphicProperties ) ;
		this.computeAbsoluteSpeedRotation( ernestGraphicProperties ) ;
	
		Matrix3f rot2 = new Matrix3f() ;
		rot2.rotZ( -ernestGraphicProperties.getmOrientation().z ) ;
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

	private void computeAbsoluteSpeedRotation( GraphicProperties ernestGraphicProperties ) {
		this.model.mSpeedR = new Vector3f( ernestGraphicProperties.getmOrientation() ) ;
		this.model.mSpeedR.sub( ernestGraphicProperties.getmPreviousOrientation() ) ;
	}

	private void computeAbsoluteSpeedTranslation( GraphicProperties ernestGraphicProperties ) {
		this.model.mSpeedT = new Vector3f( ernestGraphicProperties.getmPosition() ) ;
		this.model.mSpeedT.sub( ernestGraphicProperties.getmPreviousPosition() ) ;
	}

	protected final void turnRightAnim() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmOrientation().z -= Math.PI / 40 ;
			event.setAnimOrientation( (float) ( ernestGraphicProperties.getAnimOrientation() - ( Math.PI / 40 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		if ( ernestGraphicProperties.getmOrientation().z < -Math.PI ){
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmOrientation().z += 2 * Math.PI ;
			this.notifyGraphicPropertiesChange( event );
		}
	}

	protected final void turnLeftAnim() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmOrientation().z += Math.PI / 40 ;
			event.setAnimOrientation( (float) ( ernestGraphicProperties.getAnimOrientation() + ( Math.PI / 40 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
		
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		if ( ernestGraphicProperties.getmOrientation().z > Math.PI ){
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmOrientation().z-= 2 * Math.PI ;
			this.notifyGraphicPropertiesChange( event );
		}
	}

	protected final void bumpAheadAnim() {
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( 20 ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( 20 ) ;
		}
	}

	protected final void moveForwardAnim() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) );
			event.setAnimPosition( (float) ( ernestGraphicProperties.getAnimPosition() + .05 ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
	}

	protected final void bumpBehindAnim() {
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( 20 ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( 20 ) ;
		}
	}

	protected final void moveBackwardAnim() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent( this , ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) );
			event.setAnimPosition( (float) ( ernestGraphicProperties.getAnimPosition() - .05 ) );
			this.notifyGraphicPropertiesChange( event );
			
			this.anim() ;
			this.model.sleep( this.delayMove ) ;
		}
	}

	protected final void touchAnim() {
		this.anim() ;
		this.model.sleep( this.delayTouch ) ;
	}

	protected abstract void turnRight();
	protected abstract void turnLeft();
	protected abstract void moveForward();
	protected abstract void moveBackward();
	protected abstract void touch();
	protected abstract void touchLeft();
	protected abstract void touchRight();
}
