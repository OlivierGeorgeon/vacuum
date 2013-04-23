package ideal.vacuum.agent.motivation;

import ideal.vacuum.agent.Move ;
import ideal.vacuum.agent.TactileEffect ;
import ideal.vacuum.agent.VisualEffect ;
import utils.Pair ;
import ernest.IErnest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
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
		Pair<Move , Integer>[] moves = new Pair[3];
		moves[0] = Pair.create( Move.MOVE_FORWARD , -1 );
		moves[1] = Pair.create( Move.TURN_RIGHT , 0 );
		moves[2] = Pair.create( Move.TURN_LEFT , 0 );
		
		// r(e)
		Pair<VisualEffect , Integer>[] eyesEffects = new Pair[4];
		eyesEffects[0] = Pair.create( VisualEffect.APPEAR , 15 );
		eyesEffects[1] = Pair.create( VisualEffect.CLOSER , 10 );
		eyesEffects[2] = Pair.create( VisualEffect.UNCHANGED , 0 );
		eyesEffects[3] = Pair.create( VisualEffect.DISAPPEAR , -15 );
		
		// r(f)
		Pair<TactileEffect , Integer>[] tactileEffects = new Pair[2];
		tactileEffects[0] = Pair.create( TactileEffect.TRUE , 0 );
		tactileEffects[1] = Pair.create( TactileEffect.FALSE , -10 );
		
		
		for ( Pair<TactileEffect , Integer> tactileEffect : tactileEffects ) {
			for ( Pair<VisualEffect , Integer> eyeEffect : eyesEffects ) {
				for ( Pair<Move , Integer> move : moves ) {
					// r(u,y) = r(u) + r(f) + r(e_left) + r(e_right)
					int satisfaction = move.getRight() + tactileEffect.getRight() + eyeEffect.getRight();
					
					String stimuli = move.getLeft().getLabel() + eyeEffect.getLeft().getLabel() + tactileEffect.getLeft().getLabel() ;
					
					if( move.getLeft().equals( Move.MOVE_FORWARD ) && tactileEffect.getLeft().equals( TactileEffect.TRUE ) ){
						satisfaction = -1 + eyeEffect.getRight();
					}
					if( move.getLeft().equals( Move.MOVE_FORWARD ) && tactileEffect.getLeft().equals( TactileEffect.FALSE ) ){
						satisfaction = -8 + eyeEffect.getRight();
					}
					
					ernest.addInteraction( stimuli , satisfaction );
				}
			}
		}
	}
}
