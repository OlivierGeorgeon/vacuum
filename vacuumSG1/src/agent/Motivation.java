package agent;

import ernest.IErnest ;

public class Motivation {

	public void putMotivation( IErnest ernest ){
		// Touch wall
		ernest.addInteraction( Schema.TOUCH.getLabel() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch empty
		ernest.addInteraction( Schema.TOUCH.getLabel() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Touch right wall
		ernest.addInteraction( Schema.TOUCH_RIGHT.getLabel() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch right empty
		ernest.addInteraction( Schema.TOUCH_RIGHT.getLabel() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Touch left wall
		ernest.addInteraction( Schema.TOUCH_LEFT.getLabel() , Stimuli.TRUE.getLabel() , -2 ) ;
		// Touch left empty
		ernest.addInteraction( Schema.TOUCH_LEFT.getLabel() , Stimuli.FALSE.getLabel() , -1 ) ;
		// Move
		ernest.addInteraction( Schema.MOVE.getLabel() , Stimuli.TRUE.getLabel() , 5 ) ;
		// Bump
		ernest.addInteraction( Schema.MOVE.getLabel() , Stimuli.FALSE.getLabel() , -10 ) ;
		// Right
		ernest.addInteraction( Schema.RIGHT.getLabel() , Stimuli.TRUE.getLabel() , -3 ) ;
		// Right
		ernest.addInteraction( Schema.RIGHT.getLabel() , Stimuli.FALSE.getLabel() , -3 ) ;
		// Left
		ernest.addInteraction( Schema.LEFT.getLabel() , Stimuli.TRUE.getLabel() , -3 ) ;
		// Left
		ernest.addInteraction( Schema.LEFT.getLabel() , Stimuli.FALSE.getLabel() , -3 ) ;

		// m_ernest.addInteraction("-", "b", -1); // Touch brick
		// m_ernest.addInteraction("-", "a", -1); // Touch alga
		// m_ernest.addInteraction("\\","a", -1); // Touch right wall
		// m_ernest.addInteraction("/", "a", -1); // Touch left empty
		// m_ernest.addInteraction(">", "a", 30);// Move to alga
		// m_ernest.addInteraction("<", "t", -10); // Move backward
		// m_ernest.addInteraction("<", "f", -10);// Bump backward

		// With vision
		// m_ernest.addInteraction("-", "f", -1); // Touch empty
		// m_ernest.addInteraction("-", "t", -1); // Touch wall
		// m_ernest.addInteraction("-", "b", -1); // Touch brick
		// m_ernest.addInteraction("-", "a", -1); // Touch alga
		// m_ernest.addInteraction(">", "t", 0); // 5 Move 0
		// m_ernest.addInteraction(">", "f", -10);// Bump
		// m_ernest.addInteraction("v", "f", -3); // Right
		// m_ernest.addInteraction("^", "f", -3); // Left
	}
}
