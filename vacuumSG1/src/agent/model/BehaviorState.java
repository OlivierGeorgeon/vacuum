package agent.model;

import java.awt.Color ;

public class BehaviorState {

	private Color focusColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color leftColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color rightColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color rightEyeColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color leftEyeColor = AgentDesigner.UNANIMATED_COLOR ;
	
	public BehaviorState( Color focusColor , Color leftColor , Color rightColor , Color rightEyeColor , Color leftEyeColor ) {
		super() ;
		this.focusColor = focusColor ;
		this.leftColor = leftColor ;
		this.rightColor = rightColor ;
		this.rightEyeColor = rightEyeColor ;
		this.leftEyeColor = leftEyeColor ;
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

	public Color getRightEyeColor() {
		return this.rightEyeColor ;
	}
	
	public Color getLeftEyeColor() {
		return this.leftEyeColor ;
	}
}
