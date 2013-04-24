package ideal.vacuum.agent;

import ideal.vacuum.agent.behavior.BehaviorStateChangeEvent ;

import java.util.EventListener ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public interface DesignerListener extends EventListener{

	public void notifyGraphicPropertiesChanged( GraphicPropertiesChangeEvent properties );
	
	public void notifyBehaviorStateChanged( BehaviorStateChangeEvent behaviorState );
}
