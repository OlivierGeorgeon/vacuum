package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.Ernest130Model ;
import ideal.vacuum.agent.behavior.BehaviorState ;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Font ;
import java.awt.Graphics2D ;
import java.awt.RenderingHints ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Rectangle2D ;
import java.util.ArrayList ;

import eca.spas.Place;
import eca.spas.PlaceImpl;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class SpaceMemoryDesigner {

	protected final static int WIDTH = 300 ;
	protected final static int HEIGHT = 250 ;
	protected final static int WIDTH_REAL = WIDTH * 2 ;
	protected final static int HEIGHT_REAL = HEIGHT * 2 ;
	protected final static int SCALE = 40 ;

	private Ernest130Model model ;
	private Color agentColor ;

	public SpaceMemoryDesigner( Ernest130Model model , Color agentColor ) {
		this.model = model ;
		this.agentColor = agentColor;
	}

	public void paintSpaceMemory( Graphics2D g2d , ArrayList<Place> placeList ,
			BehaviorState behaviorState, float angleRotation , float xTranslation ) {
		g2d.setRenderingHint( RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON ) ;

		AffineTransform originLocation = g2d.getTransform() ;

		this.displayBackground( g2d ) ;
		this.displayCounter( g2d ) ;
		this.moveOriginToCenter( g2d ) ;
		
		AffineTransform centerLocation = g2d.getTransform() ;
		
		this.moveOrginForInteractions( g2d , angleRotation , xTranslation ) ;
		this.displayInteractions( g2d , placeList , behaviorState ) ;
		
		g2d.setTransform( centerLocation ) ;
		
		new AgentArrowDesigner().addAgent( g2d , this.agentColor ) ;
		
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

	private void moveOrginForInteractions( Graphics2D g2d , float angleRotation , float xTranslation ) {
		AffineTransform interactionsLocation = new AffineTransform() ;
		interactionsLocation.rotate( angleRotation ) ;
		interactionsLocation.translate( -xTranslation * SCALE , 0 ) ;
		g2d.transform( interactionsLocation ) ;
	}

	private void displayInteractions( Graphics2D g2d , ArrayList<Place> placeList ,
			BehaviorState behaviorState ) {
		for ( Place place : placeList ) {
			//if ( place.getType() == Place.ENACTION_PLACE ) {
				AffineTransform originLocation = g2d.getTransform() ;
				this.displayEnactedInteraction( g2d , place , behaviorState ) ;
				g2d.setTransform( originLocation ) ;
			//}
		}
	}

	private void displayEnactedInteraction( Graphics2D g2d , Place place , BehaviorState behaviorState ) {
		AbstractSMInteractionDesigner interactionDesigner = new VisualInteractionDesigner();
		interactionDesigner.addInteraction( g2d , place , behaviorState );
	}
}