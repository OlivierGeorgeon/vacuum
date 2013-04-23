package ideal.vacuum.agent.vision;

import java.awt.Color ;
import java.awt.Point ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Point2D ;

public class PhotoreceptorCell {

	private int xBlockPosition;
	private int yBlockPosition;
	private Color blockColor;
	
	public PhotoreceptorCell( int xBlockPosition , int yBlockPosition , Color blockColor ) {
		super() ;
		this.xBlockPosition = xBlockPosition ;
		this.yBlockPosition = yBlockPosition ;
		this.blockColor = blockColor ;
	}
	
	public int getxBlockPosition() {
		return this.xBlockPosition ;
	}
	
	public int getyBlockPosition() {
		return this.yBlockPosition ;
	}
	
	public Color getBlockColor() {
		return this.blockColor ;
	}
	
	public void orienteAxis( double theta ){
		AffineTransform aff = new AffineTransform() ;
		aff.rotate( -theta );
		Point2D blockPosition = aff.transform( new Point( this.xBlockPosition , this.yBlockPosition ), new Point() ) ;
		this.xBlockPosition = (int) blockPosition.getX(); 
		this.yBlockPosition = (int) blockPosition.getY();
	}
}
