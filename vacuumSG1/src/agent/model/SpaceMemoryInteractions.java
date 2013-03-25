package agent.model;

import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;
import java.util.HashMap ;
import java.util.Map ;

public enum SpaceMemoryInteractions {
	DEFAULT( "" , circleShape() ),
	MOVE_FORWARD( Schema.MOVE_FORWARD.getSign() + Stimuli.TRUE.getLabel() , triangleShape() ),
	BUMP( Schema.MOVE_FORWARD.getSign() + Stimuli.FALSE.getLabel() , triangleShape() ),
	TURN_LEFT_WALL( Schema.TURN_LEFT.getSign() + Stimuli.TRUE.getLabel() , arcShape() ) ,
	TURN_LEFT_EMPTY( Schema.TURN_LEFT.getSign() + Stimuli.FALSE.getLabel() , arcShape() ) ,
	TURN_RIGHT_WALL( Schema.TURN_RIGHT.getSign() + Stimuli.TRUE.getLabel() , arcShape() ) ,
	TURN_RIGHT_EMPTY( Schema.TURN_RIGHT.getSign() + Stimuli.FALSE.getLabel() , arcShape() ) ,
	TOUCH_WALL( Schema.TOUCH.getSign() + Stimuli.TRUE.getLabel() , squareShape() ),
	TOUCH_EMPTY( Schema.TOUCH.getSign() + Stimuli.FALSE.getLabel() , squareShape() ),
	TOUCH_LEFT_WALL( Schema.TOUCH_LEFT.getSign() + Stimuli.TRUE.getLabel() , squareShape() ),
	TOUCH_LEFT_EMPTY( Schema.TOUCH_LEFT.getSign() + Stimuli.FALSE.getLabel() , squareShape() ),
	TOUCH_RIGHT_WALL( Schema.TOUCH_RIGHT.getSign() + Stimuli.TRUE.getLabel() , squareShape() ) ,
	TOUCH_RIGHT_EMPTY( Schema.TOUCH_RIGHT.getSign() + Stimuli.FALSE.getLabel() , squareShape() );
	
	private final String interaction;
	private final Area shape ;
	
	private final static Map<String,SpaceMemoryInteractions> BY_INTERACTION = new HashMap<String , SpaceMemoryInteractions>();
	
	static {
		for ( SpaceMemoryInteractions smInteractions : SpaceMemoryInteractions.values() ) {
			BY_INTERACTION.put( smInteractions.interaction , smInteractions );
		}
	}
	
	private SpaceMemoryInteractions( String interaction , Area shape ){
		this.interaction = interaction;
		this.shape = shape;
	}
	
	private static Area circleShape(){
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Ellipse2D.Double( -10 , -10 , 20 , 20 ) , true );
		return new Area( shape );
	}
	
	private static Area arcShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , -90 , 180 , Arc2D.PIE ) , true ) ;
		return new Area( shape );
	}
	
	private static Area triangleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -10 , -10 , -10 , 10 ) , false ) ;
		shape.append( new Line2D.Double( -10 , 10 , 10 , 0 ) , true ) ;
		return new Area( shape );
		
	}
	
	private static Area squareShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Rectangle2D.Double( -7 , -7 , 14 , 14 ) , true );
		return new Area( shape);
	}
	
	public static SpaceMemoryInteractions getSpaceMemoryInteraction( String interaction ) {
		return BY_INTERACTION.get( interaction );
	}
	
	public String getInteraction() {
		return this.interaction ;
	}
	
	public Area getShape() {
		return this.shape ;
	}
}
