package agent.model;

import ernest.IEffect ;

public interface Behavior {

	public BehaviorState doMovement( Schema schema );

	public BehaviorState getCurrentBehaviorState() ;

	public void anim() ;

	public IEffect getEffect() ;
}
