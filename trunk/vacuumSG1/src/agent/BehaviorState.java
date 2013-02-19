package agent;

import java.awt.Color ;

import ernest.Ernest ;

public class BehaviorState {

	private float animOrientation = 0 ;
	private float animPosition = 0 ;
	
	private Color focusColor = Behavior.UNANIMATED_COLOR ;
	private Color leftColor = Behavior.UNANIMATED_COLOR ;
	private Color rightColor = Behavior.UNANIMATED_COLOR ;
	private Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA] ;
	
	public BehaviorState( float animOrientation , float animPosition , Color focusColor , Color leftColor , Color rightColor , Color[] pixelColor ) {
		super() ;
		this.animOrientation = animOrientation ;
		this.animPosition = animPosition ;
		this.focusColor = focusColor ;
		this.leftColor = leftColor ;
		this.rightColor = rightColor ;
		this.pixelColor = pixelColor ;
	}

	public float getAnimOrientation() {
		return this.animOrientation ;
	}

	public float getAnimPosition() {
		return this.animPosition ;
	}

	public Color getFocusColor() {
		return this.focusColor ;
	}

	public Color getLeftColor() {
		return this.leftColor ;
	}

	public Color getRightColor() {
		return this.rightColor ;
	}

	public Color[] getPixelColor() {
		return this.pixelColor ;
	}
}
