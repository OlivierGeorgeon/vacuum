package agent.model.behavior;

import agent.model.Move ;
import ernest.IEffect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public interface Behavior {

	public BehaviorState doMovement( Move schema );

	public BehaviorState getCurrentBehaviorState() ;

	public void anim() ;

	public IEffect getEffect() ;
}
