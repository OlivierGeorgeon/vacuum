package agent.model.motivation;

import agent.model.Schema ;
import agent.model.TactileStimuli ;
import agent.model.VisualStimuli ;
import utils.Pair ;
import ernest.IErnest ;

public class MotivationErnest8 implements Motivation{

	public void putMotivation( IErnest ernest ){
//		- [move forward, succeed, 0] Ernest is indifferent of moving forward.
//		- [move forward, fail, -8] Ernest hates bumping walls.
//		- [turn left or right, succeed, 0] Ernest is indifferent of turning toward an adjacent empty square.
//		- [turn left or right, fail, -5] Ernest dislikes turning toward an adjacent wall.
//		- [Appear, 15] Ernest loves blue squares appearing in an eye's visual field.
//		- [Closer, 10] Ernest enjoys blue squares getting closer.
//		- [Arrived, 30] Ernest is crazy about stepping on a blue square (and eating it in the process).
//		- [Disappear, -15] Ernest hates blue squares disappearing from an eye's visual field.
		
		// r(u)
		Pair<Schema , Integer>[] moves = new Pair[3];
		moves[0] = Pair.create( Schema.MOVE_FORWARD , -1 );
		moves[1] = Pair.create( Schema.TURN_RIGHT , 0 );
		moves[2] = Pair.create( Schema.TURN_LEFT , 0 );
		
		// r(e)
		Pair<VisualStimuli , Integer>[] eyesEffects = new Pair[4];
		eyesEffects[0] = Pair.create( VisualStimuli.APPEAR , 15 );
		eyesEffects[1] = Pair.create( VisualStimuli.CLOSER , 10 );
		eyesEffects[2] = Pair.create( VisualStimuli.UNCHANGED , 0 );
		eyesEffects[3] = Pair.create( VisualStimuli.DISAPPEAR , -15 );
		
		// r(f)
		Pair<TactileStimuli , Integer>[] tactileEffects = new Pair[2];
		tactileEffects[0] = Pair.create( TactileStimuli.TRUE , 0 );
		tactileEffects[1] = Pair.create( TactileStimuli.FALSE , -10 );
		
		
		for ( Pair<TactileStimuli , Integer> tactileEffect : tactileEffects ) {
			for ( Pair<VisualStimuli , Integer> leftEyeEffect : eyesEffects ) {
				for ( Pair<VisualStimuli , Integer> rightEyeEffect : eyesEffects ) {
					for ( Pair<Schema , Integer> move : moves ) {
						// r(u,y) = r(u) + r(f) + r(e_left) + r(e_right)
						int satisfaction = move.getRight() + tactileEffect.getRight() + leftEyeEffect.getRight() + rightEyeEffect.getRight();
						// y = (e_left,e_right,f)
						String stimuli = leftEyeEffect.getLeft().getLabel() + rightEyeEffect.getLeft().getLabel() + tactileEffect.getLeft().getLabel() ;
						
						if( move.getLeft().equals( Schema.MOVE_FORWARD ) && tactileEffect.getLeft().equals( TactileStimuli.TRUE ) ){
							satisfaction = 0 + leftEyeEffect.getRight() + rightEyeEffect.getRight();
						}
						if( move.getLeft().equals( Schema.MOVE_FORWARD ) && tactileEffect.getLeft().equals( TactileStimuli.FALSE ) ){
							satisfaction = -8 + leftEyeEffect.getRight() + rightEyeEffect.getRight();
						}
						
						ernest.addInteraction( move.getLeft().getLabel() , stimuli , satisfaction );
					}
				}
			}
		}
	}
}
