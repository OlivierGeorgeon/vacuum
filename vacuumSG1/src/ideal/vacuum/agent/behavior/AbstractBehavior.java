package ideal.vacuum.agent.behavior ;

import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.AgentDesigner ;
import ideal.vacuum.agent.DesignerListener ;
import ideal.vacuum.agent.GraphicProperties ;
import ideal.vacuum.agent.GraphicPropertiesChangeEvent ;
import ideal.vacuum.agent.Move ;
import ideal.vacuum.agent.vision.Eye ;
import ideal.vacuum.agent.vision.SuperiorColliculus ;

import java.awt.Color ;
import java.util.ArrayList ;
import java.util.List ;

import javax.media.j3d.Transform3D ;
import javax.swing.event.EventListenerList ;
import javax.vecmath.Vector3f ;

import eca.ActInstance;
import ernest.EffectImpl ;
import ernest.Effect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public abstract class AbstractBehavior implements Behavior {

	public static final int DELAYMOVE = 7 ; // 10
	public static final int DELAYTOUCH = 50 ; // 50

	protected Color focusColor ;
	protected Color leftColor ;
	protected Color rightColor ;

	protected Ernest130Model model ;
	protected Effect effect ;
	protected List<ActInstance> actInstances ;
	private Transform3D transform ;

	protected SuperiorColliculus colliculus ;
	
	protected EventListenerList listeners ;

	public AbstractBehavior( Ernest130Model model , DesignerListener listener , Eye eye ) {
		this.model = model ;
		this.effect = new EffectImpl() ;
		this.actInstances = new ArrayList<ActInstance>() ;
		this.transform = new Transform3D() ;
		this.listeners = new EventListenerList() ;
		this.listeners.add( DesignerListener.class , listener ) ;

		this.focusColor = AgentDesigner.UNANIMATED_COLOR ;
		this.leftColor = AgentDesigner.UNANIMATED_COLOR ;
		this.rightColor = AgentDesigner.UNANIMATED_COLOR ;
		
		this.colliculus = new SuperiorColliculus( eye );
	}

	@Override
	public final BehaviorState getCurrentBehaviorState() {
		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.colliculus.listOfMemoryAndActiveCells() ) ;
	}

	@Override
	public final Effect getEffect() {
		return this.effect ;
	}

	@Override
	public List<ActInstance> getPlaces() {
		return this.actInstances ;
	}

	@Override
	public Transform3D getTransform() {
		return this.transform ;
	}

	public void setTransform( float angle , float xTranslation ) {
		this.transform = new Transform3D() ;
		this.transform.rotZ( angle ) ;
		this.transform.setTranslation( new Vector3f( xTranslation , 0 , 0 ) ) ;
	}

	public BehaviorState doMovement( Move schema ) {
		this.effect = new EffectImpl() ;
		this.actInstances = new ArrayList<ActInstance>() ;
		
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

		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.colliculus.listOfMemoryAndActiveCells() ) ;
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

	protected final void turnRightAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z -= Math.PI / 40 ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.DELAYMOVE ) ;
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

	protected final void turnLeftAnimWorld() {
		for ( int i = 0; i < 20; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmOrientation().z += Math.PI / 40 ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( this.DELAYMOVE ) ;
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

	protected final void bumpAheadAnimWorld() {
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 2 * DELAYMOVE ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( -.05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 2 * DELAYMOVE ) ;
		}
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
			this.model.sleep( this.DELAYMOVE ) ;
		}
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
			this.model.sleep( 2 * DELAYMOVE ) ;
		}
		for ( int i = 0; i < 5; i++ ) {
			GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
			GraphicPropertiesChangeEvent event = new GraphicPropertiesChangeEvent(
					this ,
					ernestGraphicProperties ) ;
			event.getmPosition().set( this.model.localToParentRef( new Vector3f( .05f , 0 , 0 ) ) ) ;
			this.notifyGraphicPropertiesChanged( event ) ;

			this.refreshWorld() ;
			this.model.sleep( 2 * DELAYMOVE ) ;
		}
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
			this.model.sleep( this.DELAYMOVE ) ;
		}
	}

	protected final void touchAnimWorld() {
		this.refreshWorld() ;
		this.model.sleep( this.DELAYTOUCH ) ;
	}

	protected abstract void turnRight() ;

	protected abstract void turnLeft() ;

	protected abstract void moveForward() ;

	protected abstract void moveBackward() ;

	protected abstract void touch() ;

	protected abstract void touchLeft() ;

	protected abstract void touchRight() ;
}
