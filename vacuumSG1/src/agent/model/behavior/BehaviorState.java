package agent.model.behavior;

import java.awt.Color ;

import agent.model.AgentDesigner ;

public class BehaviorState {

	private Color focusColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color leftColor = AgentDesigner.UNANIMATED_COLOR ;
	private Color rightColor = AgentDesigner.UNANIMATED_COLOR ;
	private Eyes eyes;
	
	public BehaviorState( Color focusColor , Color leftColor , Color rightColor , Eyes eyes ) {
		super() ;
		this.focusColor = focusColor ;
		this.leftColor = leftColor ;
		this.rightColor = rightColor ;
		this.eyes = eyes ;
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

	public Eyes getEyes() {
		return this.eyes ;
	}
}
