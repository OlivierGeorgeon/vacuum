package agent.model.spacememory ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Font ;
import java.awt.Graphics2D ;
import java.awt.Point ;
import java.awt.RenderingHints ;
import java.awt.Shape ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Point2D ;
import java.awt.geom.Rectangle2D ;
import java.util.ArrayList ;

import spas.IPlace ;
import spas.Place ;
import agent.Ernest130Model ;
import agent.model.GraphicProperties ;
import agent.model.behavior.BehaviorState ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
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
//		this.displayAxis( g2d ); 

		this.moveOriginToCenter( g2d ) ;
		
		AffineTransform centerLocation = g2d.getTransform() ;
//		this.displayAxisTransformed( g2d );
		this.displayAgentArrowBody( g2d ) ;
		g2d.setTransform( centerLocation ) ;

		this.moveOrginForInteractions( g2d , ernestGraphicProperties ) ;
		this.displayInteractions( g2d , placeList , behaviorState ) ;
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
		g2d.setColor( Color.RED ) ;//-x;-y
		g2d.fill( new Rectangle2D.Double( 0 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.RED ) ;
		g2d.draw( new Rectangle2D.Double( 0 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;//+x;+y
		g2d.fill( new Rectangle2D.Double( WIDTH_REAL - 2 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH_REAL - 2 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;//+x;0
		g2d.fill( new Rectangle2D.Double( WIDTH_REAL - 2 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH_REAL - 2 , 0 , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;//0;+y
		g2d.fill( new Rectangle2D.Double( 0 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;
		g2d.draw( new Rectangle2D.Double( 0 , HEIGHT_REAL - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;//center
		g2d.fill( new Rectangle2D.Double( WIDTH , HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH , HEIGHT , 1 , 1 ) ) ;
	}

	private void displayAxisTransformed( Graphics2D g2d ) {
		g2d.setStroke( new BasicStroke( 4f ) ) ;
		g2d.setColor( Color.RED ) ;//-x;-y
		g2d.fill( new Rectangle2D.Double( -WIDTH , -HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.RED ) ;
		g2d.draw( new Rectangle2D.Double( -WIDTH , -HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;//+x;+y
		g2d.fill( new Rectangle2D.Double( WIDTH - 2 , HEIGHT - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.CYAN ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH - 2 , HEIGHT - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;//+x;-y
		g2d.fill( new Rectangle2D.Double( WIDTH - 2 , -HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.GRAY ) ;
		g2d.draw( new Rectangle2D.Double( WIDTH - 2 , -HEIGHT , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;//-x;+y
		g2d.fill( new Rectangle2D.Double( -WIDTH , HEIGHT - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.GREEN ) ;
		g2d.draw( new Rectangle2D.Double( -WIDTH , HEIGHT - 2 , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;//center
		g2d.fill( new Rectangle2D.Double( -50 , 30 , 1 , 1 ) ) ;
		g2d.setColor( Color.BLUE ) ;
		g2d.draw( new Rectangle2D.Double( -50 , 30 , 1 , 1 ) ) ;
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

	private void moveOrginForInteractions( Graphics2D g2d ,
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
				this.displayEnactedInteraction( g2d , place , behaviorState ) ;
				g2d.setTransform( originLocation ) ;
			}
		}
	}

	private void displayEnactedInteraction( Graphics2D g2d , IPlace place , BehaviorState behaviorState ) {
		String interactionLabel = place.getInteraction().getLabel() ;
		String move = SpaceMemoryMove.extractMoveInInteraction( interactionLabel ) ;
		SpaceMemoryMove smMove = SpaceMemoryMove.getSpaceMemoryMove( move ) ;

		Color leftInteractionColor = new Color(place.getValue());
		Color rightInteractionColor = new Color(place.getValue());
		
		if ( SpaceMemoryVisualEffect.containVisualEffect( interactionLabel ) ) {
			String leftVisualEffectLabel = SpaceMemoryVisualEffect.extractLeftVisualEffectLabel( interactionLabel ) ;
			String rightVisualEffectLabel = SpaceMemoryVisualEffect.extractRightVisualEffectLabel( interactionLabel ) ;
			leftInteractionColor = SpaceMemoryVisualEffect.getSpaceMemoryVisualEffect( leftVisualEffectLabel ).getEffectColor();
			rightInteractionColor = SpaceMemoryVisualEffect.getSpaceMemoryVisualEffect( rightVisualEffectLabel ).getEffectColor();
		}
		
		this.drawInteraction( g2d , place , smMove ,leftInteractionColor , rightInteractionColor ) ;
	}

	private void drawInteraction( Graphics2D g2d , IPlace place , SpaceMemoryMove smMove , Color leftInteractionColor, Color rightInteractionColor ) {
		Shape leftShape = smMove.getLeftHalfShape();
		Shape rightShape = smMove.getRightHalfShape();

		Point2D cartesianOffset = this.getCartesianOffset( smMove , -place.getOrientationAngle());
		
		AffineTransform interactionLocation = new AffineTransform() ;
		interactionLocation.translate(
				(int) ( place.getPosition().x * SCALE + cartesianOffset.getX() ) ,
				-(int) ( place.getPosition().y * SCALE + cartesianOffset.getY() ) ) ;
		interactionLocation.rotate( -place.getOrientationAngle() ) ;
		g2d.transform( interactionLocation ) ;
		
		g2d.setColor( leftInteractionColor ) ;
		g2d.fill( leftShape ) ;
		g2d.setColor( rightInteractionColor ) ;
		g2d.fill( rightShape ) ;
		g2d.setColor( Color.BLACK ) ;
		g2d.setStroke( new BasicStroke( SCALE / 20f ) ) ;
		g2d.draw( smMove.getShape() ) ;
	}

	private Point2D getCartesianOffset( SpaceMemoryMove smMove , float placeOrientationAngle ) {
		Point2D overlapOffset = this.getOverlapOffset( smMove ) ;
		
		double cartesianOffsetX = overlapOffset.getX() *
				Math.cos( placeOrientationAngle ) +
				overlapOffset.getY() *
				Math.sin( placeOrientationAngle ) ;
		double cartesianOffsetY = -overlapOffset.getX() *
				Math.sin( placeOrientationAngle ) +
				overlapOffset.getY() *
				Math.cos( placeOrientationAngle ) ;
		
		return new Point2D.Double( cartesianOffsetX , cartesianOffsetY );
	}

	private Point2D getOverlapOffset( SpaceMemoryMove smMove ) {
		Point overlapOffset = new Point( 0, 0 );

		switch ( smMove ) {
			case TURN_LEFT:
			case TURN_RIGHT:
				overlapOffset.x = SCALE / 4 ;
				break ;
			case TOUCH:
				overlapOffset.x = -SCALE / 3 ;
				break ;
			case TOUCH_LEFT:
				overlapOffset.x = -SCALE / 4 ;
				overlapOffset.y = -SCALE / 3 ;
				break ;
			case TOUCH_RIGHT:
				overlapOffset.x = -SCALE / 4 ;
				overlapOffset.y = SCALE / 3 ;
				break ;
			default:
				break ;
		}
		
		return overlapOffset;
	}
}