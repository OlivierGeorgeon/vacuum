package agent.model;

import ernest.IErnest ;

public class MotivationErnest7 implements Motivation{

	public void putMotivation( IErnest ernest ){
		// Touch wall
		ernest.addInteraction( Schema.TOUCH.getSign() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch empty
		ernest.addInteraction( Schema.TOUCH.getSign() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Touch right wall
		ernest.addInteraction( Schema.TOUCH_RIGHT.getSign() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch right empty
		ernest.addInteraction( Schema.TOUCH_RIGHT.getSign() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Touch left wall
		ernest.addInteraction( Schema.TOUCH_LEFT.getSign() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch left empty
		ernest.addInteraction( Schema.TOUCH_LEFT.getSign() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Move
		ernest.addInteraction( Schema.MOVE_FORWARD.getSign() , Stimuli.TRUE.getLabel() , 5 ) ;
		// Bump
		ernest.addInteraction( Schema.MOVE_FORWARD.getSign() , Stimuli.FALSE.getLabel() , -10 ) ;
		// Right
		ernest.addInteraction( Schema.TURN_RIGHT.getSign() , Stimuli.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Schema.TURN_RIGHT.getSign() , Stimuli.FALSE.getLabel() , -3 ) ;
		// Left
		ernest.addInteraction( Schema.TURN_LEFT.getSign() , Stimuli.TRUE.getLabel() , -3 ) ;
		ernest.addInteraction( Schema.TURN_LEFT.getSign() , Stimuli.FALSE.getLabel() , -3 ) ;
		// Touch brick
		ernest.addInteraction( Schema.TOUCH.getSign() , Stimuli.BRICK.getLabel() , -1 ) ;
		// Touch alga
		ernest.addInteraction( Schema.TOUCH.getSign() , Stimuli.ALGA.getLabel() , -1 ) ;
		// Touch right alga
		ernest.addInteraction( Schema.TOUCH_RIGHT.getSign() , Stimuli.ALGA.getLabel() , -1 ) ;
		// Touch left alga
		ernest.addInteraction( Schema.TOUCH_LEFT.getSign() , Stimuli.ALGA.getLabel() , -1 ) ;
		// Move to alga
		ernest.addInteraction( Schema.MOVE_FORWARD.getSign() , Stimuli.ALGA.getLabel() , 5 ) ;
	}
}
