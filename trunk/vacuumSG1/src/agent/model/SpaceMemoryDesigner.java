package agent.model ;

import imos2.IInteraction ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Font ;
import java.awt.Graphics ;
import java.awt.Graphics2D ;
import java.awt.Polygon ;
import java.awt.RenderingHints ;
import java.awt.Shape ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Point2D ;
import java.awt.geom.Rectangle2D ;
import java.util.ArrayList ;

import javax.vecmath.Point2d ;

import spas.IPlace ;
import spas.LocalSpaceMemory ;
import spas.Place ;
import agent.Ernest130Model ;

public class SpaceMemoryDesigner {

	private final int WIDTH = 300 ;
	private final int HEIGHT = 250 ;
	private final int WIDTH_REAL = WIDTH * 2 ;
	private final int HEIGHT_REAL = HEIGHT * 2 ;
	private final int SCALE = 50 ;// 50;

	private Ernest130Model model ;
	private Color agentColor ;

	public SpaceMemoryDesigner( Ernest130Model model , Color agentColor ) {
		this.model = model ;
		this.agentColor = agentColor ;
	}

	public void paintSpaceMemory( Graphics2D g2d , ArrayList<IPlace> placeList ,
			BehaviorState behaviorState ) {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;

		g2d.setRenderingHint( RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON ) ;

		AffineTransform originLocation = g2d.getTransform() ;

		this.displayBackground( g2d ) ;
		this.displayCounter( g2d ) ;
		// this.displayAxis( g2d );
		this.moveOriginToCenter( g2d , ernestGraphicProperties ) ;

		AffineTransform centerLocation = g2d.getTransform() ;
		this.displayAgentArrowBody( g2d , ernestGraphicProperties );
		g2d.setTransform( centerLocation ) ;

		this.orienteAndTranslateOrginForInteractions( g2d , ernestGraphicProperties ) ;
		this.displayInteractions( g2d , placeList ) ;
		g2d.setTransform( originLocation );

		System.out.println( "----------------------------------" ) ;
	}

	private void displayBackground( Graphics2D g2d ) {
		g2d.setColor( Color.WHITE ) ;
		g2d.fillRect( 0 , 0 , WIDTH_REAL , HEIGHT_REAL ) ;
	}

	private void displayCounter( Graphics2D g2d ) {
		String counter = this.model.getCounter() + "" ;
		g2d.setFont( new Font( "Dialog" , Font.BOLD , 18 ) ) ;
		g2d.setColor( Color.GRAY ) ;
		g2d.drawString( counter , WIDTH_REAL - 50 , 30 ) ;
	}

	private void displayAxis( Graphics2D g2d ) {
		g2d.setStroke( new BasicStroke( 4f ) ) ;
		g2d.setColor( Color.RED ) ;
		g2d.fill( new Rectangle2D.Double( 0 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.RED ) ;
		g2d.draw( new Rectangle2D.Double( 0 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;
		g2d.fill( new Rectangle2D.Double( WIDTH_REAL - 2 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH_REAL - 2 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;
		g2d.fill( new Rectangle2D.Double( WIDTH_REAL - 2 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH_REAL - 2 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;
		g2d.fill( new Rectangle2D.Double( 0 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;
		g2d.draw( new Rectangle2D.Double( 0 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;
		g2d.fill( new Rectangle2D.Double( WIDTH , HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH , HEIGHT , 1 , 1 ) ) ;
	}

	private void moveOriginToCenter( Graphics2D g2d , GraphicProperties ernestGraphicProperties ) {
		AffineTransform at = new AffineTransform() ;
		at.translate( WIDTH , HEIGHT ) ;
		at.rotate( ernestGraphicProperties.getAnimOrientation() ) ;
		g2d.transform( at ) ;
	}

	private void displayAgentArrowBody( Graphics2D g2d , GraphicProperties ernestGraphicProperties ) {
		AffineTransform placeAgent = new AffineTransform() ;
		placeAgent.scale( SCALE / 100f , SCALE / 100f ) ;
		placeAgent.rotate( -ernestGraphicProperties.getAnimOrientation() ) ;
		g2d.transform( placeAgent ) ;

		Area agent = SpaceMemoryDesigner.arrowBodyShape() ;
		g2d.setColor( this.agentColor ) ;
		g2d.fill( agent ) ;
		g2d.setStroke( new BasicStroke( SCALE / 10f ) ) ;
		g2d.setColor( Color.black ) ;
		g2d.draw( agent ) ;
	}

	private static Area arrowBodyShape() {
		GeneralPath body = new GeneralPath() ;
		body.append( new Line2D.Double( -50 , -50 , -30 , 0 ) , false ) ;
		body.append( new Line2D.Double( -30 , 0 , -50 , 50 ) , true ) ;
		body.append( new Line2D.Double( -50 , 50 , 50 , 0 ) , true ) ;

		return new Area( body ) ;
	}

	private void orienteAndTranslateOrginForInteractions( Graphics2D g2d ,
			GraphicProperties ernestGraphicProperties ) {
		AffineTransform placeAgent = new AffineTransform() ;
		placeAgent.translate( -ernestGraphicProperties.getAnimPosition() * SCALE , 0 ) ;
		g2d.transform( placeAgent ) ;
	}

	private void displayInteractions( Graphics2D g2d , ArrayList<IPlace> placeList ) {
		g2d.setStroke( new BasicStroke( SCALE / 20f ) ) ;

		for ( IPlace place : placeList ) {
			g2d.setColor( new Color( place.getValue() ) ) ;
			if ( place.getType() == Place.ENACTION_PLACE ) {
				AffineTransform originLocation = g2d.getTransform() ;
				displayEnactedInteraction( g2d , place ) ;
				g2d.setTransform( originLocation ) ;
			}
		}
	}

	private void displayEnactedInteraction( Graphics2D g2d , IPlace place ) {
		SpaceMemoryInteractions smInteraction = SpaceMemoryInteractions
				.getSpaceMemoryInteraction( place.getInteraction().getLabel() ) ;

		Shape shape = SpaceMemoryInteractions.DEFAULT.getShape() ;
		int overlapOffsetX = 0 ;
		int overlapOffsetY = 0 ;

		switch ( smInteraction ) {
			case MOVE_FORWARD:
				shape = SpaceMemoryInteractions.MOVE_FORWARD.getShape() ;
				break ;
			case BUMP:
				shape = SpaceMemoryInteractions.BUMP.getShape() ;
				break ;
			case TURN_LEFT_WALL:
				shape = SpaceMemoryInteractions.TURN_LEFT_WALL.getShape() ;
				overlapOffsetX = SCALE / 4 ;
				break ;
			case TURN_LEFT_EMPTY:
				shape = SpaceMemoryInteractions.TURN_LEFT_EMPTY.getShape() ;
				overlapOffsetX = SCALE / 4 ;
				break ;
			case TURN_RIGHT_WALL:
				shape = SpaceMemoryInteractions.TURN_RIGHT_WALL.getShape() ;
				overlapOffsetX = SCALE / 4 ;
				break ;
			case TURN_RIGHT_EMPTY:
				shape = SpaceMemoryInteractions.TURN_RIGHT_EMPTY.getShape() ;
				overlapOffsetX = SCALE / 4 ;
				break ;
			case TOUCH_WALL:
				shape = SpaceMemoryInteractions.TOUCH_WALL.getShape() ;
				overlapOffsetX = -SCALE / 3 ;
				break ;
			case TOUCH_EMPTY:
				shape = SpaceMemoryInteractions.TOUCH_EMPTY.getShape() ;
				overlapOffsetX = -SCALE / 3 ;
				break ;
			case TOUCH_LEFT_WALL:
				shape = SpaceMemoryInteractions.TOUCH_LEFT_WALL.getShape() ;
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = -SCALE / 3 ;
				break ;
			case TOUCH_LEFT_EMPTY:
				shape = SpaceMemoryInteractions.TOUCH_LEFT_EMPTY.getShape() ;
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = -SCALE / 3 ;
				break ;
			case TOUCH_RIGHT_WALL:
				shape = SpaceMemoryInteractions.TOUCH_RIGHT_WALL.getShape() ;
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = SCALE / 3 ;
			case TOUCH_RIGHT_EMPTY:
				shape = SpaceMemoryInteractions.TOUCH_RIGHT_EMPTY.getShape() ;
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = SCALE / 3 ;
				break ;
			default:
				shape = SpaceMemoryInteractions.DEFAULT.getShape() ;
				break ;
		}
		AffineTransform InteractionLocation = new AffineTransform() ;
		float cartesianOffsetX = overlapOffsetX *
				(float) Math.cos( -place.getOrientationAngle() ) +
				overlapOffsetY *
				(float) Math.sin( -place.getOrientationAngle() ) ;
		float cartesianOffsetY = -overlapOffsetX *
				(float) Math.sin( -place.getOrientationAngle() ) +
				overlapOffsetY *
				(float) Math.cos( -place.getOrientationAngle() ) ;
		InteractionLocation.translate(
				(int) ( place.getPosition().x * SCALE + cartesianOffsetX ) ,
				-(int) ( place.getPosition().y * SCALE + cartesianOffsetY ) ) ;
		InteractionLocation.rotate( -place.getOrientationAngle() ) ;
		InteractionLocation.rotate( 0 ) ;
		InteractionLocation.scale( 1 , 1 ) ;
		g2d.transform( InteractionLocation ) ;
		g2d.fill( shape ) ;
		if ( place.getType() == Place.AFFORD )
			g2d.setColor( this.agentColor ) ;
		else
			g2d.setColor( Color.black ) ;
		g2d.draw( shape ) ;
	}

}