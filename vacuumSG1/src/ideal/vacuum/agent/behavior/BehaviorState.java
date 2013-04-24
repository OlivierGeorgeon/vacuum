package ideal.vacuum.agent.behavior;

import ideal.vacuum.agent.AgentDesigner ;
import ideal.vacuum.agent.vision.Eyes ;

import java.awt.Color ;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class BehaviorState implements Cloneable{

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

	@Override
	protected BehaviorState clone() throws CloneNotSupportedException {
		BehaviorState object =  (BehaviorState) super.clone() ;
		object.eyes = this.eyes.clone();
		object.focusColor = this.focusColor;
		object.leftColor = this.leftColor;
		object.rightColor = this.rightColor;
		
		return object;
	}
}
