package agent ;

import java.awt.Color ;
import java.awt.Graphics ;
import java.awt.Graphics2D ;
import java.util.ArrayList ;

import javax.vecmath.Vector3f ;

import memory110.SpaceMemory ;
import memory110.SpaceMemoryFrame ;
import spas.IPlace ;
import tracing.XMLStreamTracer ;
import ernest.Ernest ;

/**************************************
 * A Model for a fish that moves in the environment
 * 
 * @author ogeorgeon
 **************************************/
public class Ernest130Model extends ErnestModel {

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

	public Vector3f getmPosition() {
		return this.mPosition ;
	}

	public Vector3f getmOrientation() {
		return this.mOrientation ;
	}

	public Vector3f getmTranslation() {
		return this.mTranslation ;
	}

	public Vector3f getmRotation() {
		return this.mRotation ;
	}

	public Vector3f getmPreviousPosition() {
		return this.mPreviousPosition ;
	}

	public Vector3f getmPreviousOrientation() {
		return this.mPreviousOrientation ;
	}

	public Main getMainFrame() {
		return this.mainFrame ;
	}

	public Environment getEnvironment() {
		return this.m_env ;
	}

	public void sleep( int millis ) {
		super.sleep( millis ) ;
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

		this.agentDesigner = new AgentDesigner( this , agentColor ) ;
		this.spaceMemoryDesigner = new SpaceMemoryDesigner( this ) ;
		this.behavior = new Behavior( this ) ;
		this.behaviorState = this.behavior.getCurrentBehaviorState() ;
		this.motivation = new Motivation() ;
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
		this.m_eye.setModel( this ) ;

		// Only trace the first agent.
		this.m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","NKmqGfrDVaTZQDSsgKNazjXd-cG-TZ");
		//this.m_tracer = new XMLStreamTracer( "http://134.214.128.53/abstract/lite/php/stream/" , "dvlgOqZqFcyVWdrRdJisCAqXYsttqQ" ) ;

		// Initialize the Ernest
		// Ernest's inborn primitive interactions
		this.m_ernest.setParameters( 6 , 10 ) ;

		this.m_ernest.setTracer( this.m_tracer ) ;

		this.motivation.putMotivation( this.m_ernest ) ;

		this.cognitiveMode = ErnestModel.AGENT_RUN ;

		System.out.println( "Ernest initialized" ) ;
	}

	public void setDisplay() {
		// this.setDisplaySpaceMemory() ;
		// this.setDisplayEyeView() ;
		// this.setDisplayInnerEar() ;
	}

	private void setDisplaySpaceMemory() {
		if ( !isExistFrame( Ernest130Model.SPACE_MEMORY_FRAME_CLASS_NAME ) )
			this.m_env.frameList.add( new SpaceMemoryFrame( this.spaceMemory ) ) ;
		else
			( (SpaceMemoryFrame) this.m_env.frameList.get( this.indexOfFrame( Ernest130Model.SPACE_MEMORY_FRAME_CLASS_NAME ) ) )
					.setMemory( this.spaceMemory ) ;
	}

	private void setDisplayEyeView() {
		if ( !isExistFrame( Ernest130Model.EYE_VIEW_FRAME_CLASS_NAME ) )
			this.m_env.frameList.add( new EyeView( this.m_eye ) ) ;
		else
			( (EyeView) this.m_env.frameList.get( this.indexOfFrame( Ernest130Model.EYE_VIEW_FRAME_CLASS_NAME ) ) ).setEye( this.m_eye ) ;
	}

	private void setDisplayInnerEar() {
		if ( !isExistFrame( Ernest130Model.INNER_EAR_FRAME_CLASS_NAME ) )
			this.m_env.frameList.add( new InnerEarFrame( this.m_ear ) ) ;
		else
			( (InnerEarFrame) this.m_env.frameList.get( this.indexOfFrame( Ernest130Model.INNER_EAR_FRAME_CLASS_NAME ) ) ).setInnerEar( this.m_ear ) ;
	}

	private boolean isExistFrame( String frameClassName ) {
		int frameIndex = 0 ;
		boolean isExist = false ;

		while ( frameIndex < this.m_env.frameList.size() && !isExist ) {
			System.out.println( this.m_env.frameList.get( frameIndex ).getClass().getName() ) ;
			if ( this.m_env.frameList.get( frameIndex ).getClass().getName().equals( frameClassName ) )
				isExist = true ;
			frameIndex++ ;
		}

		return isExist ;
	}

	private int indexOfFrame( String frameClassName ) {
		int frameIndex = 0 ;
		boolean isExist = false ;

		while ( frameIndex < this.m_env.frameList.size() && !isExist ) {
			if ( this.m_env.frameList.get( frameIndex ).getClass().getName().equals( frameClassName ) )
				isExist = true ;
			frameIndex++ ;
		}

		return isExist ? frameIndex - 1 : -1 ;
	}

	public void update() {
		Schema schema = Schema.valueOfByLabel( this.m_ernest.step( this.behavior.getEffect() ) ) ;

		if ( this.cognitiveMode == ErnestModel.AGENT_STEP )
			this.cognitiveMode = ErnestModel.AGENT_STOP ;

		this.behaviorState = this.behavior.doMovement( schema ) ;
		this.behavior.anim() ;
	}

	public void paintAgent( Graphics2D g2d , int x , int y , double sx , double sy ) {
		this.agentDesigner.paintAgent( g2d , x , y , sx , sy , this.behaviorState ) ;
	}

	public void paintSpaceMemory( Graphics g , ArrayList<IPlace> placeList ) {
		this.spaceMemoryDesigner.paintSpaceMemory( g , placeList , this.behaviorState ) ;
	}
}
