package ideal.vacuum ;

import ideal.vacuum.agent.AgentDesigner ;
import ideal.vacuum.agent.GraphicProperties ;
import ideal.vacuum.agent.GraphicPropertiesChangeEvent ;
import ideal.vacuum.agent.GraphicPropertiesListener ;
import ideal.vacuum.agent.Move ;
import ideal.vacuum.agent.behavior.Behavior ;
import ideal.vacuum.agent.behavior.BehaviorErnest7 ;
import ideal.vacuum.agent.behavior.BehaviorErnest8 ;
import ideal.vacuum.agent.behavior.BehaviorErnest9 ;
import ideal.vacuum.agent.behavior.BehaviorState ;
import ideal.vacuum.agent.motivation.Motivation ;
import ideal.vacuum.agent.motivation.MotivationErnest7 ;
import ideal.vacuum.agent.motivation.MotivationErnest8 ;
import ideal.vacuum.agent.spacememory.SpaceMemoryDesigner ;
import ideal.vacuum.view.MainFrame ;
import ideal.vacuum.view.SpaceMemory ;
import ideal.vacuum.view.SpaceMemoryFrame ;

import java.awt.Color ;
import java.awt.Graphics ;
import java.awt.Graphics2D ;
import java.util.ArrayList ;

import javax.vecmath.Vector3f ;

import spas.IPlace ;
import tracing.XMLStreamTracer;
import utils.ErnestUtils ;
import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class Ernest130Model extends ErnestModel implements GraphicPropertiesListener {

	private enum Version {
		ERNEST7() ,
		ERNEST8() ,
		ERNEST9() ;
	}

	private final static Ernest130Model.Version CURRENT_VERSION = Ernest130Model.Version.ERNEST9 ;
	private final static String SPACE_MEMORY_FRAME_CLASS_NAME = "memory110.SpaceMemoryFrame" ;
	private final static String EYE_VIEW_FRAME_CLASS_NAME = "agent.EyeView" ;
	private final static String INNER_EAR_FRAME_CLASS_NAME = "InnerEar" ;

	private AgentDesigner agentDesigner ;
	private SpaceMemoryDesigner spaceMemoryDesigner ;
	private Behavior behavior ;
	private BehaviorState behaviorState ;
	private Motivation motivation ;
	private SpaceMemory spaceMemory ;

	public Ernest130Model( int agentNumericalID ) {
		super( agentNumericalID ) ;
	}

	public GraphicProperties getCopyOfGraphicProperties() {
		return new GraphicProperties(
				(Vector3f) this.mPosition.clone() ,
				(Vector3f) this.mOrientation.clone() ,
				(Vector3f) this.mTranslation.clone() ,
				(Vector3f) this.mRotation.clone() ,
				(Vector3f) this.mPreviousPosition.clone() ,
				(Vector3f) this.mPreviousOrientation.clone() ) ;
	}

	public MainFrame getMainFrame() {
		return this.mainFrame ;
	}

	public Environment getEnvironment() {
		return this.m_env ;
	}

	public void sleep( int millis ) {
		super.sleep( millis ) ;
	}

	@Override
	public boolean affordCuddle( Vector3f pos ) {
		return super.affordCuddle( pos ) ;
	}

	public void init( int gridWeight , int gridHeight ) throws Exception {
		// Initialize the model
		super.init( gridWeight , gridHeight ) ;

		this.setChanged() ;
		this.notifyObservers2() ;

		Color agentColor ;
		switch ( this.ident ) {
			case 0:
				agentColor = new Color( 0xFF8000 ) ;
				break ;
			case 1:
				agentColor = Color.BLUE ;
				break ;
			default:
				agentColor = new Color( 0xFF8000 ) ;
				break ;
		}

		switch ( Ernest130Model.CURRENT_VERSION ) {
			case ERNEST7:
				this.behavior = new BehaviorErnest7( this , this ) ;
				this.motivation = new MotivationErnest7() ;
				this.agentDesigner = new AgentDesigner( this , agentColor , false , false ) ;
				break ;
			case ERNEST8:
				this.behavior = new BehaviorErnest8( this , this ) ;
				this.motivation = new MotivationErnest7() ;
				this.agentDesigner = new AgentDesigner( this , agentColor , false , false ) ;
				break ;
			case ERNEST9:
				this.behavior = new BehaviorErnest9( this , this ) ;
				this.motivation = new MotivationErnest8() ;
				this.agentDesigner = new AgentDesigner( this , agentColor , true , false ) ;
				break ;
			default:
				break ;
		}

		this.spaceMemoryDesigner = new SpaceMemoryDesigner( this , agentColor ) ;
		this.behaviorState = this.behavior.getCurrentBehaviorState() ;
		this.spaceMemory = new SpaceMemory() ;
	}

	public String getVersion() {
		return "Ernest 13.0" ;
	}

	public void initErnest() {
		// Instantiate Ernest
		this.m_ernest = new Ernest() ;

		// Initialize the visualization.
		this.spaceMemory.setModel( this ) ;

		 //Only trace the first agent.
		 this.m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","dlsQKeaXlclGbzRTN--ZLWajTDyGpr");
//		this.m_tracer = new XMLStreamTracer(
//				"http://134.214.128.53/abstract/lite/php/stream/" ,
//				"juIQzDzdCtBSpmNnJNkzdtTTajfsXe" ) ;
//		this.m_tracer = null;
		// Initialize the Ernest
		// Ernest's inborn primitive interactions
		this.m_ernest.setParameters( 6 , 10 ) ;
		this.m_ernest.setTracer( this.m_tracer ) ;
		this.motivation.putMotivation( this.m_ernest ) ;
		this.cognitiveMode = ErnestModel.AGENT_RUN ;

		System.out.println( "Ernest initialized" ) ;
	}

	public void setDisplay() {
		try {
			if( this.m_env.isPlugued( SpaceMemoryFrame.class ) ){
				this.m_env.getPlugin( SpaceMemoryFrame.class ).close();
				this.m_env.unplugFrame( SpaceMemoryFrame.class );
			}
			this.m_env.plugFrame( SpaceMemoryFrame.class );
			this.m_env.getPlugin( SpaceMemoryFrame.class ).setMemory( this.spaceMemory );
			this.m_env.getPlugin( SpaceMemoryFrame.class ).display();
		} catch ( Exception e ) {
			e.printStackTrace();
		}
	}

	public void update() {
		Move schema = Move.getByLabel( this.m_ernest.step( this.behavior.getEffect() ).substring( 0 , 1 ) );
		if ( this.cognitiveMode == ErnestModel.AGENT_STEP )
			this.cognitiveMode = ErnestModel.AGENT_STOP ;

		this.behaviorState = this.behavior.doMovement( schema ) ;
		
		this.traceEnvironmentalData() ;
		if (this.m_tracer != null)
			this.m_tracer.finishEvent();
	}

	private void traceEnvironmentalData() {
		if ( this.m_tracer != null ) {
			Object e = this.m_tracer.addEventElement( "environment" ) ;
			this.m_tracer.addSubelement( e , "x" , ErnestUtils.format( this.mPosition.x , 0 ) ) ;
			this.m_tracer.addSubelement( e , "y" , ErnestUtils.format( this.mPosition.y , 0 ) ) ;
			this.m_tracer.addSubelement(
					e ,
					"orientation" ,
					ErnestUtils.format( this.mOrientation.z , 2 ) ) ;
		}
	}

	public void paintAgent( Graphics2D g2d , int x , int y , double sx , double sy ) {
		this.agentDesigner.paintAgent( g2d , x , y , sx , sy , this.behaviorState ) ;
	}

	public void paintSpaceMemory( Graphics g , ArrayList<IPlace> placeList , float angleRotation , float xTranslation ) {
		this.spaceMemoryDesigner.paintSpaceMemory( (Graphics2D) g , placeList , this.behaviorState , angleRotation , xTranslation ) ;
	}

	@Override
	public void notifyGraphicPropertiesChanged( GraphicPropertiesChangeEvent properties ) {
		this.mPosition = properties.getmPosition() ;
		this.mOrientation = properties.getmOrientation() ;
		this.mTranslation = properties.getmTranslation() ;
		this.mRotation = properties.getmRotation() ;
		this.mPreviousPosition = properties.getmPreviousPosition() ;
		this.mPreviousOrientation = properties.getmPreviousOrientation() ;
	}
}
