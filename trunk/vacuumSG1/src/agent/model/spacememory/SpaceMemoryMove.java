package agent.model.spacememory ;

import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;
import java.util.HashMap ;
import java.util.Map ;

import agent.model.Move ;

public enum SpaceMemoryMove {
	DEFAULT( "" , circleShape() ) ,
	MOVE_FORWARD( Move.MOVE_FORWARD.getLabel() , triangleShape() ) ,
	TURN_LEFT( Move.TURN_LEFT.getLabel() , arcShape() ) ,
	TURN_RIGHT( Move.TURN_RIGHT.getLabel() , arcShape() ) ,
	TOUCH( Move.TOUCH.getLabel() , squareShape() ) ,
	TOUCH_RIGHT( Move.TOUCH_LEFT.getLabel() , squareShape() ) ,
	TOUCH_LEFT( Move.TOUCH_RIGHT.getLabel() , squareShape() ) ;
	
	private final String moveLabel ;
	private final Area shape ;

	private final static Map<String , SpaceMemoryMove> BY_MOVELABEL = new HashMap<String , SpaceMemoryMove>() ;

	static {
		for ( SpaceMemoryMove smInteractions : SpaceMemoryMove.values() ) {
			BY_MOVELABEL.put( smInteractions.moveLabel , smInteractions ) ;
		}
	}

	private SpaceMemoryMove( String moveLabel , Area shape ) {
		this.moveLabel = moveLabel ;
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

	public static SpaceMemoryMove getSpaceMemoryMove( String moveLabel ) {
		if( BY_MOVELABEL.containsKey( moveLabel ) ){
			return BY_MOVELABEL.get( moveLabel ) ;
		}else {
			return SpaceMemoryMove.DEFAULT;
		}
	}

	public static String extractMoveInInteraction( String interaction ) {
		String move = "";
		for ( char inter : interaction.toCharArray() ) {
			if ( Move.isExist( String.valueOf( inter ) ) ){
				move += inter;
				break;
			}
		}
		
		if (move.length() != 1) throw new RuntimeException( "Can't extract the move in interaction" ); 
		return move;
	}
	
	public String getMoveLabel() {
		return this.moveLabel ;
	}

	public Area getShape() {
		return this.shape ;
	}
}
