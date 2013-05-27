package ideal.vacuum.agent.behavior ;

import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.AgentDesigner ;
import ideal.vacuum.agent.DesignerListener ;
import ideal.vacuum.agent.GraphicProperties ;
import ideal.vacuum.agent.GraphicPropertiesChangeEvent ;
import ideal.vacuum.agent.Move ;
import ideal.vacuum.agent.vision.Eyes ;

import java.awt.Color ;

import javax.media.j3d.Transform3D ;
import javax.swing.event.EventListenerList ;
import javax.vecmath.Vector3f ;

import utils.ErnestUtils ;
import ernest.Effect ;
import ernest.IEffect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public abstract class AbstractBehavior implements Behavior {

	protected int delayMove = 20 ; // 2
	protected int delayTouch = 50 ; // 50

	protected Color focusColor ;
	protected Color leftColor ;
	protected Color rightColor ;
	protected Eyes eyes ;

	protected Ernest130Model model ;
	protected IEffect effect ;

	protected EventListenerList listeners ;

	public AbstractBehavior( Ernest130Model model , DesignerListener listener ) {
		this.model = model ;
		this.effect = new Effect() ;
		this.listeners = new EventListenerList() ;
		this.listeners.add( DesignerListener.class , listener ) ;

		this.focusColor = AgentDesigner.UNANIMATED_COLOR ;
		this.leftColor = AgentDesigner.UNANIMATED_COLOR ;
		this.rightColor = AgentDesigner.UNANIMATED_COLOR ;
		this.eyes = new Eyes() ;
	}

	@Override
	public final BehaviorState getCurrentBehaviorState() {
		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.eyes ) ;
	}

	@Override
	public final IEffect getEffect() {
		return this.effect ;
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
		this.notifyGraphicPropertiesChanged( event ) ;

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

		this.effect.setEnactedInteractionLabel( schema.getLabel().substring( 0 , 1 ) +
				this.effect.getLabel() ) ;
		this.refreshWorld() ;

		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.eyes ) ;
	}

	protected final void notifyGraphicPropertiesChanged( GraphicPropertiesChangeEvent event ) {
		for ( DesignerListener listener : this.listeners.getListeners( DesignerListener.class ) ) {
			listener.notifyGraphicPropertiesChanged( event ) ;
		}
	}

	protected final void notifyBehaviorStateChanged( BehaviorStateChangeEvent event ) {
		for ( DesignerListener listener : this.listeners.getListeners( DesignerListener.class ) ) {
			listener.notifyBehaviorStateChanged( event ) ;
		}
	}

	private final void refreshWorld() {
		this.model.getMainFrame().drawGrid() ;
	}

	public final void refreshFramesPlugins( final float angleRotation , final float xTranslation ) {
		Transform3D transformation = this.model.getErnest().getTransformToAnim() ;
		//System.out.println("Anim rotation: " + ErnestUtils.angle( transformation ) + " translation " + ErnestUtils.translationX( transformation ));
		this.model.getEnvironment().animFramesPlugins(
				- ErnestUtils.angle( transformation ) ,
				- ErnestUtils.translationX( transformation ) ) ;
//		this.model.getEnvironment().animFramesPlugins(
//				angleRotation ,
//				xTranslation ) ;
	}

	protected final void turnRightAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z -= Math.PI / 40 ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.delayMove ) ;
		}

		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		if ( ernestGraphicProperties.getmOrientation().z < -Math.PI ) {
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z += 2 * Math.PI ;
			this.notifyGraphicPropertiesChanged( event ) ;
		}
	}

	protected final void turnRightAnimFramesPlugins() {
		this.refreshFramesPlugins( (float) - ( Math.PI / 2 ) , 0 ) ;
	}

	protected final void turnLeftAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z += Math.PI / 40 ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.delayMove ) ;
		}

		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		if ( ernestGraphicProperties.getmOrientation().z > Math.PI ) {
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z -= 2 * Math.PI ;
			this.notifyGraphicPropertiesChanged( event ) ;
		}
	}

	protected final void turnLeftAnimFramesPlugins() {
		this.refreshFramesPlugins( (float) ( Math.PI / 2 ) , 0 ) ;
	}

	protected final void bumpAheadAnimWorld() {
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 20 ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 20 ) ;
		}
	}

	protected final void bumpAheadAnimFramesPlugins() {
		this.refreshFramesPlugins( 0 , 0 ) ;
	}

	protected final void moveForwardAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.delayMove ) ;
		}
	}

	protected final void moveForwardAnimFramesPlugins() {
		this.refreshFramesPlugins( 0 , 1 ) ;
	}

	protected final void bumpBehindAnimWorld() {
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 20 ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 20 ) ;
		}
	}

	protected final void bumpBehindAnimFramesPlugins() {
		this.refreshFramesPlugins( 0 , 0 ) ;
	}

	protected final void moveBackwardAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.delayMove ) ;
		}
	}

	protected final void moveBackwardAnimFramesPlugins() {
		this.refreshFramesPlugins( 0 , -1 ) ;
	}

	protected final void touchAnimWorld() {
		this.refreshWorld() ;
		this.model.sleep( this.delayTouch ) ;
	}

	protected final void touchAnimFramesPlugins() {
		this.refreshFramesPlugins( 0 , 0 ) ;
	}

	protected abstract void turnRight() ;

	protected abstract void turnLeft() ;

	protected abstract void moveForward() ;

	protected abstract void moveBackward() ;

	protected abstract void touch() ;

	protected abstract void touchLeft() ;

	protected abstract void touchRight() ;
}
