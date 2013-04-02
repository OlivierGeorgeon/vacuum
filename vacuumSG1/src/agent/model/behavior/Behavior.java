package agent.model.behavior;

import agent.model.Move ;
import ernest.IEffect ;

public interface Behavior {

	public BehaviorState doMovement( Move schema );

	public BehaviorState getCurrentBehaviorState() ;

	public void anim() ;

	public IEffect getEffect() ;
}
