package agent;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.Polygon ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Area ;
import java.awt.geom.CubicCurve2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Rectangle2D ;

import ernest.Ernest ;

public class AgentDesigner {

	private Ernest130Model model ;
	private Color agentColor ;
	
	public AgentDesigner( Ernest130Model model , Color agentColor ) {
		this.model = model;
		this.agentColor = agentColor;
	}
	
	public void paintAgent( Graphics2D g2d , int x , int y , double sx , double sy , BehaviorState behaviorState )	{
		// The orientation
		AffineTransform orientation = new AffineTransform() ;
		orientation.translate( x , y ) ;
		orientation.rotate( -this.model.getmOrientation().z + Math.PI / 2 ) ;
		orientation.scale( sx , sy ) ;
		g2d.transform( orientation ) ;

		// The shark body
//        Area shark = Designer.shape(ident);
        
		// Retina pixel
		Rectangle2D.Double focus = new Rectangle2D.Double( -12 , -40 , 25 , 30 ) ;
		Rectangle2D.Double left = new Rectangle2D.Double( -35 , -10 , 25 , 30 ) ;
		Rectangle2D.Double right = new Rectangle2D.Double( 10 , -10 , 25 , 30 ) ;

		// Draw the body
		AffineTransform ref0 = g2d.getTransform() ;
		AffineTransform r = new AffineTransform() ;
		r.rotate( -Math.PI / 2 ) ;
		r.scale( .8 , .8 ) ;
		g2d.transform( r ) ;

		Polygon agent = new Polygon() ;
		agent.addPoint( -50 , -40 ) ;
		agent.addPoint( -30 , 0 ) ;
		agent.addPoint( -50 , 40 ) ;
		agent.addPoint( 50 , 0 ) ;
		g2d.setColor( this.agentColor ) ;
		g2d.fill( agent ) ;
		g2d.setStroke( new BasicStroke( 4f ) ) ;
		g2d.setColor( Color.black ) ;
		g2d.draw( agent ) ;
		g2d.setTransform( ref0 ) ;

		// Draw the focus
		g2d.setStroke( new BasicStroke( 2f ) ) ;
		g2d.setColor( behaviorState.getLeftColor() ) ;
		if ( behaviorState.getLeftColor() != Behavior.UNANIMATED_COLOR ){
			g2d.fill( left ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( left ) ;
		}
		g2d.setColor( behaviorState.getRightColor() ) ;
		if ( behaviorState.getRightColor() != Behavior.UNANIMATED_COLOR ){
			g2d.fill( right ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( right ) ;
		}
		if ( behaviorState.getFocusColor() != Behavior.UNANIMATED_COLOR )	{
			g2d.setColor( behaviorState.getFocusColor() ) ;
			g2d.fill( focus ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( focus ) ;
		}

		// Draw the retina
		AffineTransform transformColliculus = new AffineTransform() ;
		transformColliculus.rotate( 0 ) ;
		transformColliculus.translate( 0 , 0 ) ;
		g2d.transform( transformColliculus ) ;
		AffineTransform RetinaReference = g2d.getTransform() ;
		AffineTransform transformSegment = new AffineTransform() ;
		g2d.transform( transformSegment ) ;
		transformSegment.rotate( -Math.PI / Ernest.RESOLUTION_RETINA ) ;
		g2d.setColor( Color.BLACK ) ;

		g2d.setTransform( RetinaReference ) ;
		for ( int i = 0; i < Ernest.RESOLUTION_RETINA; i++ )
		{
			g2d.setColor( behaviorState.getPixelColor()[i] ) ;
			g2d.transform( transformSegment ) ;
		}

	}
	
	/**
	 * The shape is centered in (0,0) and fits in a 100x100 rectangle. The
	 * pelvic fin pattern represents the agent's id number in binary code.
	 * 
	 * @param ID
	 *            The agent's id number.
	 * @return The shark area.
	 */
	private static Area shape( int ID ){
		GeneralPath body = new GeneralPath() ;
		body.append( new CubicCurve2D.Double( 0 , -40 , -30 , -40 , -5 , 45 , 0 , 45 ) , false ) ;
		body.append( new CubicCurve2D.Double( 0 , 45 , 5 , 45 , 30 , -40 , 0 , -40 ) , false ) ;

		GeneralPath leftPectoralFin = new GeneralPath() ;
		leftPectoralFin.append( new CubicCurve2D.Double( -15 , -15 , -30 , -10 , -40 , 0 , -40 , 20 ) , false ) ;
		leftPectoralFin.append( new CubicCurve2D.Double( -40 , 20 , -30 , 10 , -20 , 8 , -10 , 10 ) , true ) ;

		GeneralPath leftPelvicFin = new GeneralPath() ;
		leftPelvicFin.append( new CubicCurve2D.Double( -10 , 15 , -15 , 18 , -20 , 25 , -15 , 30 ) , false ) ;
		leftPelvicFin.append( new CubicCurve2D.Double( -15 , 30 , -10 , 25 , -10 , 25 , -5 , 28 ) , true ) ;

		GeneralPath rightPectoralFin = new GeneralPath() ;
		rightPectoralFin.append( new CubicCurve2D.Double( 15 , -15 , 30 , -10 , 40 , 0 , 40 , 20 ) , false ) ;
		rightPectoralFin.append( new CubicCurve2D.Double( 40 , 20 , 30 , 10 , 20 , 8 , 10 , 10 ) , true ) ;

		GeneralPath rightPelvicFin = new GeneralPath() ;
		rightPelvicFin.append( new CubicCurve2D.Double( 10 , 15 , 15 , 18 , 20 , 25 , 15 , 30 ) , false ) ;
		rightPelvicFin.append( new CubicCurve2D.Double( 15 , 30 , 10 , 25 , 10 , 25 , 5 , 28 ) , true ) ;

		GeneralPath caudalFin = new GeneralPath() ;
		caudalFin.append( new CubicCurve2D.Double( 10 , 50 , 15 , 20 , -15 , 20 , -10 , 50 ) , false ) ;
		caudalFin.append( new CubicCurve2D.Double( -10 , 50 , -15 , 30 , 15 , 30 , 10 , 50 ) , false ) ;

		Area shark = new Area( body ) ;
		shark.add( new Area( leftPectoralFin ) ) ;
		if ( ( ID & 1 ) == 0 )
			shark.add( new Area( leftPelvicFin ) ) ;
		shark.add( new Area( rightPectoralFin ) ) ;
		if ( ( ID & 2 ) == 0 )
			shark.add( new Area( rightPelvicFin ) ) ;

		return shark ;
	}
}
