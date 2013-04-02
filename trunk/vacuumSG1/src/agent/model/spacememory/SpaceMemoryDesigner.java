package agent.model.spacememory ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Font ;
import java.awt.Graphics2D ;
import java.awt.RenderingHints ;
import java.awt.Shape ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;
import java.util.ArrayList ;

import spas.IPlace ;
import spas.Place ;
import agent.Ernest130Model ;
import agent.model.GraphicProperties ;
import agent.model.behavior.BehaviorState ;
import agent.model.behavior.Eyes ;

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

		this.moveOriginToCenter( g2d ) ;

		AffineTransform centerLocation = g2d.getTransform() ;
		this.displayAgentArrowBody( g2d ) ;
		g2d.setTransform( centerLocation ) ;

		this.orienteAndTranslateOrginForInteractions( g2d , ernestGraphicProperties ) ;
		this.displayInteractions( g2d , placeList , behaviorState ) ;
//		g2d.setTransform( centerLocation ) ;
//
//		if ( !placeList.isEmpty() )
//			this.displayVisualsInteractions(
//					g2d ,
//					ernestGraphicProperties ,
//					placeList.get( placeList.size() - 1 ) ,
//					behaviorState.getEyes() ) ;
		g2d.setTransform( originLocation ) ;

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

	private void moveOriginToCenter( Graphics2D g2d ) {
		AffineTransform centerLocation = new AffineTransform() ;
		centerLocation.translate( WIDTH , HEIGHT ) ;
		g2d.transform( centerLocation ) ;
	}

	private void displayAgentArrowBody( Graphics2D g2d ) {
		AffineTransform agentLocation = new AffineTransform() ;
		agentLocation.scale( SCALE / 100f , SCALE / 100f ) ;
		g2d.transform( agentLocation ) ;

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
		AffineTransform interactionsLocation = new AffineTransform() ;
		interactionsLocation.rotate( ernestGraphicProperties.getAnimOrientation() ) ;
		interactionsLocation.translate( -ernestGraphicProperties.getAnimPosition() * SCALE , 0 ) ;
		g2d.transform( interactionsLocation ) ;
	}

	private void displayInteractions( Graphics2D g2d , ArrayList<IPlace> placeList ,
			BehaviorState behaviorState ) {
		for ( IPlace place : placeList ) {
			if ( place.getType() == Place.ENACTION_PLACE ) {
				AffineTransform originLocation = g2d.getTransform() ;
				this.displayEnactedInteractions( g2d , place ) ;
				g2d.setTransform( originLocation ) ;
			}
		}
	}

	private void displayEnactedInteractions( Graphics2D g2d , IPlace place ) {
		String move = SpaceMemoryMove
				.extractMoveInInteraction( place.getInteraction().getLabel() ) ;
		SpaceMemoryMove smMove = SpaceMemoryMove
				.getSpaceMemoryMove( move ) ;

		Shape shape = smMove.getShape() ;
		int overlapOffsetX = 0 ;
		int overlapOffsetY = 0 ;

		switch ( smMove ) {
			case TURN_LEFT:
			case TURN_RIGHT:
				overlapOffsetX = SCALE / 4 ;
				break ;
			case TOUCH:
				overlapOffsetX = -SCALE / 3 ;
				break ;
			case TOUCH_LEFT:
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = -SCALE / 3 ;
				break ;
			case TOUCH_RIGHT:
				overlapOffsetX = -SCALE / 4 ;
				overlapOffsetY = SCALE / 3 ;
				break ;
			default:
				break ;
		}
		AffineTransform interactionLocation = new AffineTransform() ;
		float cartesianOffsetX = overlapOffsetX *
				(float) Math.cos( -place.getOrientationAngle() ) +
				overlapOffsetY *
				(float) Math.sin( -place.getOrientationAngle() ) ;
		float cartesianOffsetY = -overlapOffsetX *
				(float) Math.sin( -place.getOrientationAngle() ) +
				overlapOffsetY *
				(float) Math.cos( -place.getOrientationAngle() ) ;
		interactionLocation.translate(
				(int) ( place.getPosition().x * SCALE + cartesianOffsetX ) ,
				-(int) ( place.getPosition().y * SCALE + cartesianOffsetY ) ) ;
		interactionLocation.rotate( -place.getOrientationAngle() ) ;
		g2d.transform( interactionLocation ) ;
		g2d.setColor( new Color( place.getValue() ) ) ;
		g2d.fill( shape ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.setStroke( new BasicStroke( SCALE / 20f ) ) ;
		g2d.draw( shape ) ;
	}

	private void displayVisualsInteractions( Graphics2D g2d ,
			GraphicProperties ernestGraphicProperties , IPlace lastEnactedPlace , Eyes eyes ) {
		if ( SpaceMemoryVisualInteractions.containVisualInteraction( lastEnactedPlace
				.getInteraction().getLabel() ) ) {
			AffineTransform originLocation = g2d.getTransform() ;
			String visualInteraction = SpaceMemoryVisualInteractions
					.extractVisualInteraction( lastEnactedPlace.getInteraction().getLabel() ) ;

			SpaceMemoryVisualInteractions smVisualInteraction = SpaceMemoryVisualInteractions
					.getSpaceMemoryVisualInteraction(
							visualInteraction ,
							eyes.getLeftEyeDistanceToTheblock() ,
							eyes.getRightEyeDistanceToTheblock() ) ;
			this.displayLeftEyeVisualInteraction( g2d , eyes , smVisualInteraction ) ;
			g2d.setTransform( originLocation ) ;

			this.displayRightEyeVisualInteraction( g2d , eyes , smVisualInteraction ) ;
			g2d.setTransform( originLocation ) ;
		}
	}

	private void displayRightEyeVisualInteraction( Graphics2D g2d , Eyes eyes ,
			SpaceMemoryVisualInteractions smVisualInteraction ) {
		AffineTransform rightVisualInteractionLocation = new AffineTransform() ;
		rightVisualInteractionLocation.translate( WIDTH_REAL / 4 , HEIGHT_REAL / 4 ) ;
		rightVisualInteractionLocation.scale( SCALE / 1000f , SCALE / 1000f ) ;
		g2d.transform( rightVisualInteractionLocation ) ;

		g2d.setStroke( new BasicStroke( SCALE / 1f ) ) ;
		g2d.setColor( eyes.getRightEyeLookedBlock() ) ;
		g2d.fill( smVisualInteraction.getRightEyeShape() ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.draw( smVisualInteraction.getRightEyeShape() ) ;
	}

	private void displayLeftEyeVisualInteraction( Graphics2D g2d , Eyes eyes ,
			SpaceMemoryVisualInteractions smVisualInteraction ) {
		AffineTransform leftVisualInteractionLocation = new AffineTransform() ;
		leftVisualInteractionLocation.translate( WIDTH_REAL / 4 , -HEIGHT_REAL / 4 ) ;
		leftVisualInteractionLocation.scale( SCALE / 1000f , SCALE / 1000f ) ;
		g2d.transform( leftVisualInteractionLocation ) ;

		g2d.setStroke( new BasicStroke( SCALE / 1f ) ) ;
		g2d.setColor( eyes.getLeftEyeLookedBlock() ) ;
		g2d.fill( smVisualInteraction.getLeftEyeShape() ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.draw( smVisualInteraction.getLeftEyeShape() ) ;
	}
}