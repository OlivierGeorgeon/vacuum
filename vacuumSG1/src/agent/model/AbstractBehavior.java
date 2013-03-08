package agent.model;

import java.awt.Color ;

import javax.swing.event.EventListenerList ;
import javax.vecmath.Matrix3f ;
import javax.vecmath.Vector3f ;

import agent.Ernest130Model ;
import ernest.Effect ;
import ernest.Ernest ;
import ernest.IEffect ;

public abstract class AbstractBehavior implements Behavior {

	public static Color UNANIMATED_COLOR = Color.GRAY ;
	
	protected int delayMove = 5 ; // 2
	protected int delayTouch = 50 ; // 50
	
	protected Color focusColor = UNANIMATED_COLOR ;
	protected Color leftColor = UNANIMATED_COLOR ;
	protected Color rightColor = UNANIMATED_COLOR ;
	protected Color[] retinaPixelsColors = new Color[Ernest.RESOLUTION_RETINA] ;
	
	protected Ernest130Model model ;
	protected IEffect effect ;
	
	protected EventListenerList listeners;
	
	public AbstractBehavior( Ernest130Model model , GraphicPropertiesListener listener ) {
		this.model = model ;
		this.effect = new Effect() ;
		this.listeners = new EventListenerList();
		this.listeners.add( GraphicPropertiesListener .class, listener);
		this.focusColor = BehaviorErnest7.UNANIMATED_COLOR ;
		this.leftColor = BehaviorErnest7.UNANIMATED_COLOR ;
		this.rightColor = BehaviorErnest7.UNANIMATED_COLOR ;
	}
	
	@Override
	public final BehaviorState getCurrentBehaviorState() {
		return new BehaviorState( this.focusColor , this.leftColor , this.rightColor , this.retinaPixelsColors ) ;
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
	
	@Override
	public final void anim() {
		// compute absolute movements
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

}
