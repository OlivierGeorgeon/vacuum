package agent.model;

import java.awt.Color ;


import ernest.Ernest ;

public class BehaviorState {

	private Color focusColor = BehaviorErnest7.UNANIMATED_COLOR ;
	private Color leftColor = BehaviorErnest7.UNANIMATED_COLOR ;
	private Color rightColor = BehaviorErnest7.UNANIMATED_COLOR ;
	private Color[] retinaPixelsColors = new Color[Ernest.RESOLUTION_RETINA] ;
	
	public BehaviorState( Color focusColor , Color leftColor , Color rightColor , Color[] retinaPixelsColors ) {
		super() ;
		this.focusColor = focusColor ;
		this.leftColor = leftColor ;
		this.rightColor = rightColor ;
		this.retinaPixelsColors = retinaPixelsColors ;
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

	public Color[] getRetinaPixelsColors() {
		return this.retinaPixelsColors ;
	}
}
