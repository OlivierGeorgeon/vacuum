package ideal.vacuum.agent.vision;

import java.awt.Color ;
import java.awt.Point ;
import java.awt.geom.AffineTransform ;
import java.awt.geom.Point2D ;

import ernest.Ernest ;

public class PhotoreceptorCell implements Cloneable  {

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
	
	public double distanceAccurateToTheBlock() {
		if ( Math.abs( this.xBlockPosition ) == Ernest.INFINITE ||
				Math.abs( this.yBlockPosition ) == Ernest.INFINITE )
			return Ernest.INFINITE ;
		return this.distanceToTheBlock() * Ernest.INT_FACTOR ;
	}

	public double distanceToTheBlock() {
		return this.calculateHypotenuse();
	}
	
	private double calculateHypotenuse() {
		return ( Math.sqrt( this.xBlockPosition * this.xBlockPosition + this.yBlockPosition *
				this.yBlockPosition ) ) ;
	}
	
	@Override
	public PhotoreceptorCell clone() throws CloneNotSupportedException {
		PhotoreceptorCell object = (PhotoreceptorCell) super.clone() ;
		object.xBlockPosition = this.xBlockPosition ;
		object.yBlockPosition = this.yBlockPosition ;
		object.blockColor = this.blockColor ;
		return object ;
	}
}
