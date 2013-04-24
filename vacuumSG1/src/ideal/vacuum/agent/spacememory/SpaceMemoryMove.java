package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.Move ;

import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;
import java.util.HashMap ;
import java.util.Map ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public enum SpaceMemoryMove {
	DEFAULT(
			"" ,
			circleShape() ,
			leftHalfCircleShape() ,
			rightHalfCircleShape() ) ,
	MOVE_FORWARD(
			Move.MOVE_FORWARD.getLabel() ,
			squareShape() ,
			leftHalfSquareShape() ,
			rightHalfSquareShape() ) ,
	TURN_LEFT(
			Move.TURN_LEFT.getLabel() ,
			trapezoidShape() ,
			leftHalfTrapezoidShape() ,
			rightHalfTrapezoidShape() ) ,
	TURN_RIGHT(
			Move.TURN_RIGHT.getLabel() ,
			arcShape() ,
			leftHalfTrapezoidShape() ,
			rightHalfTrapezoidShape() ) ,
	TOUCH(
			Move.TOUCH.getLabel() ,
			squareShape() ,
			leftHalfSquareShape() ,
			rightHalfSquareShape() ) ,
	TOUCH_RIGHT(
			Move.TOUCH_RIGHT.getLabel() ,
			squareShape() ,
			leftHalfSquareShape() ,
			rightHalfSquareShape() ) ,
	TOUCH_LEFT(
			Move.TOUCH_LEFT.getLabel() ,
			squareShape() ,
			leftHalfSquareShape() ,
			rightHalfSquareShape() ) ;

	private final String moveLabel ;
	private final Area shape ;
	private final Area leftHalfShape ;
	private final Area rightHalfShape ;

	private final static Map<String , SpaceMemoryMove> BY_MOVE_LABEL = new HashMap<String , SpaceMemoryMove>() ;

	static {
		for ( SpaceMemoryMove smInteractions : SpaceMemoryMove.values() ) {
			BY_MOVE_LABEL.put( smInteractions.moveLabel , smInteractions ) ;
		}
	}

	private SpaceMemoryMove( String moveLabel ,
			Area shape ,
			Area leftHalfShape ,
			Area rightHalfShape ) {
		this.moveLabel = moveLabel ;
		this.shape = shape ;
		this.leftHalfShape = leftHalfShape ;
		this.rightHalfShape = rightHalfShape ;
	}

	private static Area circleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Ellipse2D.Double( -10 , -10 , 20 , 20 ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area leftHalfCircleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , 0 , 180 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area rightHalfCircleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , 180 , 180 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area arcShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , -180 , 180 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area leftHalfArcShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , -90 , 90 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area rightHalfArcShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Arc2D.Double( -10 , -10 , 20 , 20 , -180 , 90 , Arc2D.PIE ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area triangleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -10 , -10 , 10 , 0 ) , false ) ;
		shape.append( new Line2D.Double( 10 , 0 , -10 , 10 ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area leftHalfTriangleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -10 , -10 , 10 , 0 ) , false ) ;
		shape.append( new Line2D.Double( 10 , 0 , -10 , 0 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area rightHalfTriangleShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -10 , 0 , 10 , 0 ) , false ) ;
		shape.append( new Line2D.Double( 10 , 0 , -10 , 10 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area trapezoidShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -7 , 14 , 7 , 7 ) , false ) ;
		shape.append( new Line2D.Double( 7 , 7 , 7 , -7 ) , true ) ;
		shape.append( new Line2D.Double( 7 , -7 , -7 , 0 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area leftHalfTrapezoidShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -7 , 7 , 7 , 0 ) , false ) ;
		shape.append( new Line2D.Double( 7 , 0 , 7 , -7 ) , true ) ;
		shape.append( new Line2D.Double( 7 , -7 , -7 , 0 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area rightHalfTrapezoidShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Line2D.Double( -7 , 14 , 7 , 7 ) , false ) ;
		shape.append( new Line2D.Double( 7 , 7 , 7 , 0 ) , true ) ;
		shape.append( new Line2D.Double( 7 , 0 , -7 , 7 ) , true ) ;
		return new Area( shape ) ;

	}

	private static Area squareShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Rectangle2D.Double( -7 , -7 , 14 , 14 ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area leftHalfSquareShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Rectangle2D.Double( -7 , -7 , 14 , 7 ) , true ) ;
		return new Area( shape ) ;
	}

	private static Area rightHalfSquareShape() {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Rectangle2D.Double( -7 , 0 , 14 , 7 ) , true ) ;
		return new Area( shape ) ;
	}

	public static SpaceMemoryMove getSpaceMemoryMove( String moveLabel ) {
		if ( BY_MOVE_LABEL.containsKey( moveLabel ) ) {
			return BY_MOVE_LABEL.get( moveLabel ) ;
		} else {
			return SpaceMemoryMove.DEFAULT ;
		}
	}

	public static String extractMoveLabel( String interaction ) {
		for ( char inter : interaction.toCharArray() ) {
			if ( Move.isExist( String.valueOf( inter ) ) ) {
				return String.valueOf( inter ) ;
			}
		}

		throw new RuntimeException( "Can't extract the move in interaction : " + interaction ) ;
	}

	public String getMoveLabel() {
		return this.moveLabel ;
	}

	public Area getShape() {
		return this.shape ;
	}

	public Area getLeftHalfShape() {
		return this.leftHalfShape ;
	}

	public Area getRightHalfShape() {
		return this.rightHalfShape ;
	}
}
