package ideal.vacuum.agent.behavior;

import ideal.vacuum.agent.Move ;

import java.util.List ;

import javax.media.j3d.Transform3D ;

import eca.ActInstance;
import ernest.Effect ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public interface Behavior {

	public BehaviorState doMovement( Move schema );

	public BehaviorState getCurrentBehaviorState() ;

	public Effect getEffect() ;

	public Transform3D getTransform() ;

	public List<ActInstance> getPlaces() ;
}
