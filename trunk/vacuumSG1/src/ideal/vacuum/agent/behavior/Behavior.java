package ideal.vacuum.agent.behavior;

import ideal.vacuum.agent.Move ;
import ernest.IEffect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public interface Behavior {

	public BehaviorState doMovement( Move schema );

	public BehaviorState getCurrentBehaviorState() ;

	public IEffect getEffect() ;
}
