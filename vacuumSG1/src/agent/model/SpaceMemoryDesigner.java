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
import java.awt.geom.Ellipse2D ;
import java.util.ArrayList ;

import spas.IPlace ;
import spas.LocalSpaceMemory ;
import spas.Place ;
import agent.Ernest130Model ;

public class SpaceMemoryDesigner {

	private Ernest130Model model ;

	public SpaceMemoryDesigner( Ernest130Model model ) {
		this.model = model ;
	}

	public void paintSpaceMemory( Graphics g , ArrayList<IPlace> placeList , BehaviorState behaviorState ) {
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;
		// ArrayList<IPlace> placeList = new ArrayList<IPlace>();

		// placeList = getErnest().getPlaceList();

		final int WIDTH = 300 ;
		final int HEIGHT = 250 ;
		final int SCALE = 50 ;// 40;
		Color agentColor = new Color( 0xFF8000 ) ;

		boolean displayPhenomenon = true ;

		Graphics2D g2d = (Graphics2D) g ;

		g2d.setRenderingHint( RenderingHints.KEY_ANTIALIASING , RenderingHints.VALUE_ANTIALIAS_ON ) ;

		// Display background
		// g2d.setColor(new Color(200, 200, 200));
		g2d.setColor( Color.white ) ;
		g2d.fillRect( 0 , 0 , 2 * WIDTH , 2 * HEIGHT ) ;

		// Display counter
		String counter = this.model.getCounter() + "" ;
		Font font = new Font( "Dialog" , Font.BOLD , 18 ) ;
		g2d.setFont( font ) ;
		// FontMetrics fm = getFontMetrics(font);
		// int width = fm.stringWidth(counter);
		g2d.setColor( Color.GRAY ) ;
		// g2d.drawString(counter, 2 * WIDTH - 30 - width, 30);
		g2d.drawString( counter , 2 * WIDTH - 50 , 30 ) ;

		float agentOrientation = 0 ;
		// for (IPlace place : spaceMemory.getPlaceList())
		// {
		// if (place.getType() == Spas.PLACE_FOCUS)
		// {
		// refAngle = place.getDirection();
		// }
		// }

		// IPlace focusPlace = getErnest().getFocusPlace();
		agentOrientation = this.model.getOrientation() ;

		// float baseOrientation = - agentOrientation +
		// focusPlace.getOrientation();
		float baseOrientation = agentOrientation ; // allocentric
		// float baseOrientation = - focusPlace.getOrientation(); // target
		// centric.
		// float baseOrientation = 0; // agent horizontal
		// float x = (float)Math.cos(baseOrientation +
		// focusPlace.getDirection()) * focusPlace.getDistance();
		// float y = (float)Math.sin(baseOrientation +
		// focusPlace.getDirection()) * focusPlace.getDistance();

		AffineTransform ref0 = g2d.getTransform() ;
		AffineTransform ref1 = new AffineTransform() ;
		// ref1.translate( - x * SCALE, y * SCALE);
		// ref1.rotate(- baseOrientation, WIDTH, HEIGHT);
		ref1.translate( -ernestGraphicProperties.getAnimPosition() * SCALE , 0 ) ;
		ref1.rotate( ernestGraphicProperties.getAnimOrientation() , WIDTH , HEIGHT ) ;
		g2d.transform( ref1 ) ;
		// g2d.setTransform(ref0);

		// Reference transformation
		AffineTransform ref = g2d.getTransform() ;

		// g2d.setTransform(ref);

		double d ;
		double rad ;
		double angle ;
		double span ;

		// Display the places
		// g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_BUTT,
		// BasicStroke.JOIN_MITER));
		g2d.setStroke( new BasicStroke( SCALE / 3f , BasicStroke.CAP_ROUND , BasicStroke.JOIN_MITER ) ) ;

		Ellipse2D.Double circle = new Ellipse2D.Double( -10 , -10 , 20 , 20 ) ;
		Arc2D.Double pie = new Arc2D.Double( -10 , -10 , 20 , 20 , -90 , 180 , Arc2D.PIE ) ;
		Polygon triangle = new Polygon() ;
		triangle.addPoint( -10 , -10 ) ;
		triangle.addPoint( -10 , 10 ) ;
		triangle.addPoint( 10 , 0 ) ;
		int squareSize = 7 ;
		Polygon square = new Polygon() ;
		square.addPoint( -squareSize , -squareSize ) ;
		square.addPoint( -squareSize , squareSize ) ;
		square.addPoint( squareSize , squareSize ) ;
		square.addPoint( squareSize , -squareSize ) ;
		// Polygon tile = new
		// Polygon();tile.addPoint(-SCALE/2,-SCALE/2);tile.addPoint(-SCALE/2,SCALE/2);tile.addPoint(SCALE/2,SCALE/2);tile.addPoint(SCALE/2,-SCALE/2);
		Ellipse2D.Double tile = new Ellipse2D.Double( -SCALE * .4 , -SCALE * .4 , SCALE * .8 , SCALE * .8 ) ;
		Polygon agent = new Polygon() ;
		agent.addPoint( -50 , -50 ) ;
		agent.addPoint( -30 , 0 ) ;
		agent.addPoint( -50 , 50 ) ;
		agent.addPoint( 50 , 0 ) ;

		// Display the phenomenon
		g2d.setStroke( new BasicStroke( SCALE / 20f ) ) ;
		AffineTransform or ;

		// //////////////
		for ( IPlace place : placeList )
		{
			if ( place.getType() == Place.EVOKED_PLACE )
			{
				// g2d.setColor(new Color(place.getValue()));
				int scale = (int) ( 127 + 128 * ( this.model.getUpdateCount() - place.getClock() ) / (float) LocalSpaceMemory.PERSISTENCE_DURATION ) ;
				if ( scale > 255 )
					scale = 255 ;
				g2d.setColor( new Color( scale , scale , scale ) ) ;

				ref = g2d.getTransform() ;
				or = new AffineTransform() ;
				or.translate( WIDTH + (int) ( place.getPosition().x * SCALE ) , HEIGHT - (int) ( place.getPosition().y * SCALE ) ) ;
				// or.scale(( 1 - (getUpdateCount() -
				// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),(
				// 1 - (getUpdateCount() -
				// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
				// or.rotate(- place.getOrientation());
				g2d.transform( or ) ;
				// g2d.fill(tile);
				g2d.setColor( new Color( 0xC0C0C0 ) ) ;
				// if (place.getBundle().getValue() == Ernest.PHENOMENON_WALL)
				// g2d.setColor(new Color(place.getValue()));
				// g2d.draw(tile);
				g2d.setTransform( ref ) ;

				// for (IAct a : place.getBundle().getActList())
				// IAct a = place.getAct();
				IInteraction a = place.getInteraction() ;
				{
					// g2d.setColor(new Color(a.getColor()));
					g2d.setColor( new Color( place.getValue() ) ) ;
					Shape shape = circle ;
					int offsetx = 0 ;
					int offsety = 0 ;
					float orientation = 0 ;
					if ( a.getLabel().indexOf( ">" ) >= 0 )
					{
						shape = triangle ;
						// if (a.getLabel().equals(">f"))
						// g2d.setColor(new Color(255, 80, 80));
					}
					if ( a.getLabel().equals( "^f" ) )
					{
						shape = pie ;
						orientation = (float) -Math.PI / 2 ;
						offsety = SCALE / 4 ;
					}
					if ( a.getLabel().equals( "vf" ) )
					{
						shape = pie ;
						orientation = (float) Math.PI / 2 ;
						offsety = -SCALE / 4 ;
					}
					if ( a.getLabel().equals( "/f" ) || a.getLabel().equals( "/t" ) )
					{
						shape = square ;
						offsetx = -SCALE / 4 ;
						offsety = -SCALE / 3 ;
					}
					if ( a.getLabel().equals( "\\f" ) || a.getLabel().equals( "\\t" ) )
					{
						shape = square ;
						offsetx = -SCALE / 4 ;
						offsety = SCALE / 3 ;
					}
					if ( a.getLabel().equals( "-f" ) || a.getLabel().equals( "-t" ) )
					{
						shape = square ;
						offsetx = -SCALE / 3 ;
					}
					if ( a.getLabel().equals( "-b" ) )
					{
						shape = square ;
						offsetx = -SCALE / 3 ;
						offsety = SCALE / 6 ;
					}

					if ( shape != circle )
					{
						ref = g2d.getTransform() ;
						or = new AffineTransform() ;
						float ooffx = offsetx * (float) Math.cos( -place.getOrientationAngle() ) + offsety
								* (float) Math.sin( -place.getOrientationAngle() ) ;
						float ooffy = -offsetx * (float) Math.sin( -place.getOrientationAngle() ) + offsety
								* (float) Math.cos( -place.getOrientationAngle() ) ;
						or.translate( WIDTH + (int) ( place.getPosition().x * SCALE + ooffx ) , HEIGHT
								- (int) ( place.getPosition().y * SCALE + ooffy ) ) ;
						// or.translate(WIDTH + (int)(place.getPosition().x *
						// SCALE + offsetx), HEIGHT -
						// (int)(place.getPosition().y * SCALE + offsety));
						// or.scale(( 1 - (getUpdateCount() -
						// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),(
						// 1 - (getUpdateCount() -
						// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
						or.rotate( -place.getOrientationAngle() ) ;
						or.rotate( orientation ) ;
						g2d.transform( or ) ;
						g2d.fill( shape ) ;
						g2d.setColor( Color.gray ) ;
						g2d.draw( shape ) ;
						g2d.setTransform( ref ) ;
					}
				}
			}
		}
		// ///////////////////////////////

		// Display agent
		g2d.setStroke( new BasicStroke( SCALE / 10f ) ) ;
		g2d.setTransform( ref ) ;
		AffineTransform placeAgent = new AffineTransform() ;
		placeAgent.translate( WIDTH + ernestGraphicProperties.getAnimPosition() * SCALE , HEIGHT ) ;
		// placeAgent.rotate(Math.PI/2);
		placeAgent.scale( SCALE / 100f , SCALE / 100f ) ;
		placeAgent.rotate( -ernestGraphicProperties.getAnimOrientation() ) ;
		g2d.transform( placeAgent ) ;
		g2d.setColor( agentColor ) ;
		// g2d.fill(shape(getID()));
		g2d.fill( agent ) ;
		g2d.setColor( Color.black ) ;
		g2d.draw( agent ) ;
		g2d.setTransform( ref ) ;

		// Display the interactions
		g2d.setStroke( new BasicStroke( SCALE / 20f ) ) ;
		// AffineTransform or;
		// for (IPlace place : getErnest().getPlaceList())
		for ( IPlace place : placeList )
		{
			if ( place.getType() == Place.ENACTION_PLACE || place.getType() == Place.AFFORD || place.getType() == Place.UNKNOWN )// ||
																																	// place.getType()
																																	// ==
																																	// Place.EVOKED_PLACE)
			{
				g2d.setColor( new Color( place.getValue() ) ) ;

				Shape shape = circle ;
				float orientation = 0 ;
				int offsetx = 0 ;
				int offsety = 0 ;
				float scale = 1 ;
				// IAct a = place.getAct();
				IInteraction a = place.getInteraction() ;
				if ( a != null )
				{
					if ( a.getLabel().indexOf( ">" ) >= 0 )
						shape = triangle ;

					if ( a.getLabel().equals( "^f" ) )
					{
						shape = pie ;
						orientation = 0 ;// (float) - Math.PI;// / 2;
						offsetx = SCALE / 4 ;
					}
					if ( a.getLabel().equals( "^* f" ) )
					{
						shape = pie ;
						orientation = 0 ;// (float) - Math.PI;// / 2;
						offsetx = SCALE / 4 ;
					}
					if ( a.getLabel().equals( "vf" ) )
					{
						shape = pie ;
						orientation = 0 ; // (float) Math.PI / 2;
						offsetx = SCALE / 4 ;
					}
					if ( a.getLabel().equals( "v *f" ) )
					{
						shape = pie ;
						orientation = 0 ; // (float) Math.PI / 2;
						offsetx = SCALE / 4 ;
					}
					if ( a.getLabel().indexOf( "/" ) >= 0 )
					{
						shape = square ;
						offsetx = -SCALE / 4 ;
						offsety = -SCALE / 3 ;
					}
					if ( a.getLabel().indexOf( "\\" ) >= 0 )
					{
						shape = square ;
						offsetx = -SCALE / 4 ;
						offsety = SCALE / 3 ;
					}
					if ( a.getLabel().equals( "-f" ) || a.getLabel().equals( "-t" ) )
					{
						shape = square ;
						offsetx = -SCALE / 3 ;
					}
					if ( a.getLabel().equals( "-b" ) )
					{
						shape = square ;
						offsetx = -SCALE / 3 ;
					}
					if ( a.getLabel().equals( "-a" ) )
					{
						shape = square ;
						offsetx = -SCALE / 3 ;
					}
					if ( a.getLabel().equals( "(^f>t)" ) || a.getLabel().equals( "(vf>t)" ) )
					{
						shape = triangle ;
						orientation = 0 ;
						offsetx = 0 ;
						offsety = 0 ;
						g2d.setColor( Color.yellow ) ;
					}
					if ( a.getLabel().equals( "(-f>t)" ) )
					{
						shape = triangle ;
						orientation = 0 ;
						offsetx = 0 ;
						offsety = 0 ;
						g2d.setColor( Color.pink ) ;
					}
				}
				if ( shape != circle )
				{
					ref = g2d.getTransform() ;
					or = new AffineTransform() ;
					float ooffx = offsetx * (float) Math.cos( -place.getOrientationAngle() ) + offsety
							* (float) Math.sin( -place.getOrientationAngle() ) ;
					float ooffy = -offsetx * (float) Math.sin( -place.getOrientationAngle() ) + offsety
							* (float) Math.cos( -place.getOrientationAngle() ) ;
					or.translate( WIDTH + (int) ( place.getPosition().x * SCALE + ooffx ) , HEIGHT - (int) ( place.getPosition().y * SCALE + ooffy ) ) ;
					// or.translate(WIDTH + (int)(place.getPosition().x * SCALE
					// + offsetx), HEIGHT - (int)(place.getPosition().y * SCALE
					// + offsety ));
					// or.scale(( 1 - (getUpdateCount() -
					// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),(
					// 1 - (getUpdateCount() -
					// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
					or.rotate( -place.getOrientationAngle() ) ;
					or.rotate( orientation ) ;
					or.scale( scale , scale ) ;
					g2d.transform( or ) ;
					g2d.fill( shape ) ;
					if ( place.getType() == Place.AFFORD )
						g2d.setColor( agentColor ) ;
					else if ( place.getType() == Place.UNKNOWN )
						g2d.setColor( new Color( 0x8080FF ) ) ;
					else if ( place.getType() == Place.EVOKED_PLACE )
						g2d.setColor( new Color( 0xFF0000 ) ) ;
					else
						g2d.setColor( Color.black ) ;
					g2d.draw( shape ) ;
					g2d.setTransform( ref ) ;
					// g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1 -
					// (spaceMemory.getUpdateCount() -
					// place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND,
					// BasicStroke.JOIN_ROUND));
				}
			}
		}
		// for (IPlace place : placeList)
		// {
		// if (place.getType() == Spas.PLACE_SIMULATION && place.getAct() ==
		// null)
		// {
		// g2d.setColor(Color.red);
		//
		// Shape shape = circle;
		// float orientation = 0;
		// int offsetx = 0;
		// int offsety = 0;
		//
		// ref = g2d.getTransform();
		// or = new AffineTransform();
		// float ooffx = offsetx * (float)Math.cos(- place.getOrientation()) +
		// offsety * (float)Math.sin(- place.getOrientation());
		// float ooffy = - offsetx * (float)Math.sin(- place.getOrientation()) +
		// offsety * (float)Math.cos(- place.getOrientation());
		// or.translate(WIDTH + (int)(place.getPosition().x * SCALE + ooffx),
		// HEIGHT - (int)(place.getPosition().y * SCALE + ooffy));
		// //or.translate(WIDTH + (int)(place.getPosition().x * SCALE +
		// offsetx), HEIGHT - (int)(place.getPosition().y * SCALE + offsety ));
		// //or.scale(( 1 - (getUpdateCount() -
		// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),(
		// 1 - (getUpdateCount() -
		// place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
		// or.rotate(- place.getOrientation());
		// or.rotate(orientation);
		// g2d.transform(or);
		// g2d.fill(shape);
		// g2d.setColor(Color.black);
		// g2d.draw(shape);
		// g2d.setTransform(ref);
		// //g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1 -
		// (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1),
		// BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
		// }
		// }
	}
}
