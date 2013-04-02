package agent.model.motivation;

import agent.model.Move ;
import agent.model.TactileEffect ;
import ernest.IErnest ;

public class MotivationErnest7 implements Motivation{

	public void putMotivation( IErnest ernest ){
		// Touch wall
		ernest.addInteraction( Move.TOUCH.getLabel() , TactileEffect.TRUE.getLabel() , -2 ) ;
		// Touch empty
		ernest.addInteraction( Move.TOUCH.getLabel() , TactileEffect.FALSE.getLabel() , -1 ) ;
		// Touch right wall
		ernest.addInteraction( Move.TOUCH_RIGHT.getLabel() , TactileEffect.TRUE.getLabel() , -2 ) ;
		// Touch right empty
		ernest.addInteraction( Move.TOUCH_RIGHT.getLabel() , TactileEffect.FALSE.getLabel() , -1 ) ;
		// Touch left wall
		ernest.addInteraction( Move.TOUCH_LEFT.getLabel() , TactileEffect.TRUE.getLabel() , -2 ) ;
		// Touch left empty
		ernest.addInteraction( Move.TOUCH_LEFT.getLabel() , TactileEffect.FALSE.getLabel() , -1 ) ;
		// Move
		ernest.addInteraction( Move.MOVE_FORWARD.getLabel() , TactileEffect.TRUE.getLabel() , 5 ) ;
		// Bump
		ernest.addInteraction( Move.MOVE_FORWARD.getLabel() , TactileEffect.FALSE.getLabel() , -10 ) ;
		// Right
		ernest.addInteraction( Move.TURN_RIGHT.getLabel() , TactileEffect.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Move.TURN_RIGHT.getLabel() , TactileEffect.FALSE.getLabel() , -3 ) ;
		// Left
		ernest.addInteraction( Move.TURN_LEFT.getLabel() , TactileEffect.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Move.TURN_LEFT.getLabel() , TactileEffect.FALSE.getLabel() , -3 ) ;
		// Touch brick
		ernest.addInteraction( Move.TOUCH.getLabel() , TactileEffect.BRICK.getLabel() , -1 ) ;
		// Touch alga
		ernest.addInteraction( Move.TOUCH.getLabel() , TactileEffect.ALGA.getLabel() , -1 ) ;
		// Touch right alga
		ernest.addInteraction( Move.TOUCH_RIGHT.getLabel() , TactileEffect.ALGA.getLabel() , -1 ) ;
		// Touch left alga
		ernest.addInteraction( Move.TOUCH_LEFT.getLabel() , TactileEffect.ALGA.getLabel() , -1 ) ;
		// Move to alga
		ernest.addInteraction( Move.MOVE_FORWARD.getLabel() , TactileEffect.ALGA.getLabel() , 5 ) ;
	}
}
