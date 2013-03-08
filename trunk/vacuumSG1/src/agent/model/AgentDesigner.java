package agent.model;

import java.awt.BasicStroke ;
import java.awt.Color ;
import java.awt.Graphics2D ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Arc2D ;
import java.awt.geom.Area ;
import java.awt.geom.CubicCurve2D ;
import java.awt.geom.GeneralPath ;
import java.awt.geom.Line2D ;
import java.awt.geom.Rectangle2D ;

import agent.Ernest130Model ;
import ernest.Ernest ;

public class AgentDesigner {

	private Ernest130Model model ;
	private Color agentColor ;
	private boolean useRetina ;
	private boolean useSharkBody ;
	
	public AgentDesigner( Ernest130Model model , Color agentColor , boolean useRetina , boolean useSharkBody ) {
		this.model = model;
		this.agentColor = agentColor;
		this.useRetina = useRetina;
		this.useSharkBody = useSharkBody;
	}
	
	public void paintAgent( Graphics2D g2d , int x , int y , double sx , double sy , BehaviorState behaviorState )	{
		GraphicProperties ernestGraphicProperties = this.model.getCopyOfGraphicProperties() ;

		this.drawAgentOrientation( g2d , x , y , sx , sy , ernestGraphicProperties ) ;

		if ( this.useSharkBody ) {
			this.drawAgentSharkBody( g2d );
		} else {
			this.drawAgentArrowBody( g2d ) ;
		}
		
		if( this.useRetina ){
			this.drawAgentRetina( g2d , behaviorState ) ;
		}
		else{
			this.drawAgentFocus( g2d , behaviorState ) ;
		}
	}

	private void drawAgentRetina( Graphics2D g2d , BehaviorState behaviorState ) {
		Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
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
		for ( int i = 0; i < Ernest.RESOLUTION_RETINA; i++ ){
			g2d.setColor( behaviorState.getRetinaPixelsColors()[i] ) ;
			g2d.fill(pixelIn);
			g2d.transform( transformSegment ) ;
		}
	}

	private void drawAgentFocus( Graphics2D g2d , BehaviorState behaviorState ) {
		Rectangle2D.Double focus = new Rectangle2D.Double( -12 , -40 , 25 , 30 ) ;
		Rectangle2D.Double left = new Rectangle2D.Double( -35 , -10 , 25 , 30 ) ;
		Rectangle2D.Double right = new Rectangle2D.Double( 10 , -10 , 25 , 30 ) ;
		g2d.setStroke( new BasicStroke( 2f ) ) ;
		g2d.setColor( behaviorState.getLeftColor() ) ;
		if ( behaviorState.getLeftColor() != BehaviorErnest7.UNANIMATED_COLOR ){
			g2d.fill( left ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( left ) ;
		}
		g2d.setColor( behaviorState.getRightColor() ) ;
		if ( behaviorState.getRightColor() != BehaviorErnest7.UNANIMATED_COLOR ){
			g2d.fill( right ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( right ) ;
		}
		if ( behaviorState.getFocusColor() != BehaviorErnest7.UNANIMATED_COLOR ){
			g2d.setColor( behaviorState.getFocusColor() ) ;
			g2d.fill( focus ) ;
			g2d.setColor( Color.black ) ;
			g2d.draw( focus ) ;
		}
	}

	private void drawAgentSharkBody( Graphics2D g2d ){
		Area agent = AgentDesigner.sharkBodyShape();
        g2d.setColor( this.agentColor );
        g2d.fill( agent );
        g2d.setStroke( new BasicStroke( 4f ) ) ;
        g2d.setColor( Color.black );
        g2d.draw( agent );
	}
	
	private void drawAgentArrowBody( Graphics2D g2d ) {
		AffineTransform ref0 = g2d.getTransform() ;
		AffineTransform r = new AffineTransform() ;
		r.rotate( -Math.PI / 2 ) ;
		r.scale( .8 , .8 ) ;
		g2d.transform( r ) ;

		Area agent = AgentDesigner.arrowBodyShape() ;
		g2d.setColor( this.agentColor ) ;
		g2d.fill( agent ) ;
		g2d.setStroke( new BasicStroke( 4f ) ) ;
		g2d.setColor( Color.black ) ;
		g2d.draw( agent ) ;
		g2d.setTransform( ref0 ) ;
	}

	private void drawAgentOrientation( Graphics2D g2d , int x , int y , double sx , double sy , GraphicProperties ernestGraphicProperties ) {
		AffineTransform orientation = new AffineTransform() ;
		orientation.translate( x , y ) ;
		orientation.rotate( -ernestGraphicProperties.getmOrientation().z + Math.PI / 2 ) ;
		orientation.scale( sx , sy ) ;
		g2d.transform( orientation ) ;
	}
	
	private static Area arrowBodyShape(){
		GeneralPath body = new GeneralPath() ;
		body.append( new Line2D.Double( -50 , -40 , -30 , 0 ) , false );
		body.append( new Line2D.Double( -30 , 0 , -50 , 40 ) , true );
		body.append( new Line2D.Double( -50 , 40 , 50 , 0 ) , true );
		
		return new Area( body );
	}
	
	private static Area sharkBodyShape(){
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
		shark.add( new Area( rightPectoralFin ) ) ;
		shark.add( new Area( leftPelvicFin ) ) ;
		shark.add( new Area( rightPelvicFin ) ) ;

		return shark ;
	}
}
