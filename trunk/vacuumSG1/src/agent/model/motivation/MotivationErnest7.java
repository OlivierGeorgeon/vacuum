package agent.model.motivation;

import agent.model.Schema ;
import agent.model.TactileStimuli ;
import ernest.IErnest ;

public class MotivationErnest7 implements Motivation{

	public void putMotivation( IErnest ernest ){
		// Touch wall
		ernest.addInteraction( Schema.TOUCH.getLabel() , TactileStimuli.TRUE.getLabel() , -2 ) ;
		// Touch empty
		ernest.addInteraction( Schema.TOUCH.getLabel() , TactileStimuli.FALSE.getLabel() , -1 ) ;
		// Touch right wall
		ernest.addInteraction( Schema.TOUCH_RIGHT.getLabel() , TactileStimuli.TRUE.getLabel() , -2 ) ;
		// Touch right empty
		ernest.addInteraction( Schema.TOUCH_RIGHT.getLabel() , TactileStimuli.FALSE.getLabel() , -1 ) ;
		// Touch left wall
		ernest.addInteraction( Schema.TOUCH_LEFT.getLabel() , TactileStimuli.TRUE.getLabel() , -2 ) ;
		// Touch left empty
		ernest.addInteraction( Schema.TOUCH_LEFT.getLabel() , TactileStimuli.FALSE.getLabel() , -1 ) ;
		// Move
		ernest.addInteraction( Schema.MOVE_FORWARD.getLabel() , TactileStimuli.TRUE.getLabel() , 5 ) ;
		// Bump
		ernest.addInteraction( Schema.MOVE_FORWARD.getLabel() , TactileStimuli.FALSE.getLabel() , -10 ) ;
		// Right
		ernest.addInteraction( Schema.TURN_RIGHT.getLabel() , TactileStimuli.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Schema.TURN_RIGHT.getLabel() , TactileStimuli.FALSE.getLabel() , -3 ) ;
		// Left
		ernest.addInteraction( Schema.TURN_LEFT.getLabel() , TactileStimuli.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Schema.TURN_LEFT.getLabel() , TactileStimuli.FALSE.getLabel() , -3 ) ;
		// Touch brick
		ernest.addInteraction( Schema.TOUCH.getLabel() , TactileStimuli.BRICK.getLabel() , -1 ) ;
		// Touch alga
		ernest.addInteraction( Schema.TOUCH.getLabel() , TactileStimuli.ALGA.getLabel() , -1 ) ;
		// Touch right alga
		ernest.addInteraction( Schema.TOUCH_RIGHT.getLabel() , TactileStimuli.ALGA.getLabel() , -1 ) ;
		// Touch left alga
		ernest.addInteraction( Schema.TOUCH_LEFT.getLabel() , TactileStimuli.ALGA.getLabel() , -1 ) ;
		// Move to alga
		ernest.addInteraction( Schema.MOVE_FORWARD.getLabel() , TactileStimuli.ALGA.getLabel() , 5 ) ;
	}
}
