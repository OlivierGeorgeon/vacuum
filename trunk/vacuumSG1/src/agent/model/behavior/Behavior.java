package agent.model.behavior;

import agent.model.Schema ;
import ernest.IEffect ;

public interface Behavior {

	public BehaviorState doMovement( Schema schema );

	public BehaviorState getCurrentBehaviorState() ;

	public void anim() ;

	public IEffect getEffect() ;
}
