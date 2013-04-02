package agent.model.spacememory ;

import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;
import java.util.HashMap ;
import java.util.Map ;

import agent.model.Schema ;
import agent.model.TactileStimuli ;

public enum SpaceMemoryPhysicalInteraction {
	DEFAULT( "" , circleShape() ) ,
	MOVE_FORWARD( Schema.MOVE_FORWARD.getLabel() + TactileStimuli.TRUE.getLabel() , triangleShape() ) ,
	BUMP( Schema.MOVE_FORWARD.getLabel() + TactileStimuli.FALSE.getLabel() , triangleShape() ) ,
	TURN_LEFT_WALL( Schema.TURN_LEFT.getLabel() + TactileStimuli.TRUE.getLabel() , arcShape() ) ,
	TURN_LEFT_EMPTY( Schema.TURN_LEFT.getLabel() + TactileStimuli.FALSE.getLabel() , arcShape() ) ,
	TURN_RIGHT_WALL( Schema.TURN_RIGHT.getLabel() + TactileStimuli.TRUE.getLabel() , arcShape() ) ,
	TURN_RIGHT_EMPTY( Schema.TURN_RIGHT.getLabel() + TactileStimuli.FALSE.getLabel() , arcShape() ) ,
	TOUCH_WALL( Schema.TOUCH.getLabel() + TactileStimuli.TRUE.getLabel() , squareShape() ) ,
	TOUCH_EMPTY( Schema.TOUCH.getLabel() + TactileStimuli.FALSE.getLabel() , squareShape() ) ,
	TOUCH_LEFT_WALL( Schema.TOUCH_LEFT.getLabel() + TactileStimuli.TRUE.getLabel() , squareShape() ) ,
	TOUCH_LEFT_EMPTY( Schema.TOUCH_LEFT.getLabel() + TactileStimuli.FALSE.getLabel() , squareShape() ) ,
	TOUCH_RIGHT_WALL( Schema.TOUCH_RIGHT.getLabel() + TactileStimuli.TRUE.getLabel() , squareShape() ) ,
	TOUCH_RIGHT_EMPTY( Schema.TOUCH_RIGHT.getLabel() + TactileStimuli.FALSE.getLabel() , squareShape() ) ;

	private final String interaction ;
	private final Area shape ;

	private final static Map<String , SpaceMemoryPhysicalInteraction> BY_PHYSICAL_INTERACTION = new HashMap<String , SpaceMemoryPhysicalInteraction>() ;

	static {
		for ( SpaceMemoryPhysicalInteraction smInteractions : SpaceMemoryPhysicalInteraction.values() ) {
			BY_PHYSICAL_INTERACTION.put( smInteractions.interaction , smInteractions ) ;
		}
	}

	private SpaceMemoryPhysicalInteraction( String interaction , Area shape ) {
		this.interaction = interaction ;
		this.shape = shape ;
	}

	private static Area circleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Ellipse2D.Double( -10 , -10 , 20 , 20 ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area arcShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , -90 , 180 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area triangleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -10 , -10 , -10 , 10 ) , false ) ;
		shape.append( new Line2D.Double( -10 , 10 , 10 , 0 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area squareShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Rectangle2D.Double( -7 , -7 , 14 , 14 ) , true ) ;
		return new Area( shape ) ;
	}

	public static SpaceMemoryPhysicalInteraction getSpaceMemoryPhysicalInteraction( String physicalInteraction ) {
		if( BY_PHYSICAL_INTERACTION.containsKey( physicalInteraction ) ){
			return BY_PHYSICAL_INTERACTION.get( physicalInteraction ) ;
		}else {
			return SpaceMemoryPhysicalInteraction.DEFAULT;
		}
	}

	public static String extractPhysicalInteraction( String interaction ) {
		String physicalInteraction = "";
		for ( char inter : interaction.toCharArray() ) {
			if ( Schema.isExist( String.valueOf( inter ) ) ){
				physicalInteraction += inter;
				break;
			}
		}
		for ( char inter : interaction.toCharArray() ) {
			if ( TactileStimuli.isExist( String.valueOf( inter ) ) ){
				physicalInteraction += inter;
				break;
			}
		}
		
		if (physicalInteraction.length() != 2) throw new RuntimeException( "Can't extract the physical interaction" ); 
		return physicalInteraction;
	}
	
	public String getInteraction() {
		return this.interaction ;
	}

	public Area getShape() {
		return this.shape ;
	}
}
