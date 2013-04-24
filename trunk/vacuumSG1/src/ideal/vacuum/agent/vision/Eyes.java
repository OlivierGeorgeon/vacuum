package ideal.vacuum.agent.vision ;

import ideal.vacuum.Environment ;
import ideal.vacuum.agent.VisualEffect;

import java.awt.Color ;

import javax.vecmath.Point3f;

import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class Eyes implements Cloneable {

	public static int RESOLUTION_RETINA = 1;
	public static final float DISTANCE_VISION = 4;

	private PhotoreceptorCell photoreceptor;
	private PhotoreceptorCell previousPhotoreceptorState;
	
	public Eyes() {
		this.photoreceptor = new PhotoreceptorCell( Ernest.INFINITE , Ernest.INFINITE , Environment.empty.color );
		this.previousPhotoreceptorState = this.photoreceptor;
	}

	public void updateEye( int xBlockPosition , int yBlockPosition , Color blockColor ) {
		this.previousPhotoreceptorState = this.photoreceptor ;
		this.photoreceptor = new PhotoreceptorCell( xBlockPosition , yBlockPosition , blockColor );
	}

	
	public int getxBlockPosition() {
		return this.photoreceptor.getxBlockPosition() ;
	}

	public int getyBlockPosition() {
		return this.photoreceptor.getyBlockPosition() ;
	}
	
	public VisualEffect visualEffect() {
		VisualEffect stimuli = VisualEffect.UNCHANGED ;
		float previousDistance = previousPhotoreceptorState.distanceAccurateToTheBlock();
		float currentDistance = photoreceptor.distanceAccurateToTheBlock();

		if ( previousDistance == currentDistance ) {
			stimuli = VisualEffect.UNCHANGED ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance ) {
			stimuli = VisualEffect.CLOSER ;
		} else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE ) {
			stimuli = VisualEffect.APPEAR ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE ) {
			stimuli = VisualEffect.DISAPPEAR ;
		}

//		System.out.println( "Sensed " +
//				"prev=" +
//				previousDistance +
//				" cur=" +
//				currentDistance +
//				" stimuli " +
//				stimuli.getLabel() ) ;

		return stimuli ;
	}

	public Point3f getEventPosition()
	{
		float d = 0;
		Point3f position = new Point3f();
		VisualEffect stimuli = visualEffect() ;
		
		switch (stimuli) {
		case CLOSER: 
		case APPEAR:
			position = this.photoreceptor.getBlockPosition();
			d = this.photoreceptor.distanceAccurateToTheBlock();
			if (d > 0 && d < Ernest.INFINITE)
				position.scale(DISTANCE_VISION / d);
			break;
		case DISAPPEAR:
			position = this.previousPhotoreceptorState.getBlockPosition();
			d = this.previousPhotoreceptorState.distanceAccurateToTheBlock();
			if (d > 0 && d < Ernest.INFINITE)
				position.scale(DISTANCE_VISION / d);
			break;
		}

		return position ;
	}

	public Color getBlockColor() {
		return this.photoreceptor.getBlockColor() ;
	}

//	public float distanceAccurateToTheBlock() {
//		return this.photoreceptor.distanceAccurateToTheBlock() ;
//	}

//	public float distanceToTheBlock() {
//		return this.photoreceptor.distanceToTheBlock() ;
//	}

	public Eyes takeSnapshot() {
		try {
			return this.clone() ;
		} catch ( CloneNotSupportedException e ) {
			e.printStackTrace() ;
		}
		return null ;
	}

	public boolean isActif( PhotoreceptorCell previousPhotoreceptorState , PhotoreceptorCell currentPhotoreceptorState ) {
		boolean isActif = false ;

		if ( previousPhotoreceptorState.distanceAccurateToTheBlock() == currentPhotoreceptorState.distanceAccurateToTheBlock() ) {
			isActif = false ;
		} else if ( previousPhotoreceptorState.distanceAccurateToTheBlock() < Ernest.INFINITE &&
				currentPhotoreceptorState.distanceAccurateToTheBlock() < previousPhotoreceptorState.distanceAccurateToTheBlock() ) {
			isActif = true ;
		} else if ( previousPhotoreceptorState.distanceAccurateToTheBlock() == Ernest.INFINITE &&
				currentPhotoreceptorState.distanceAccurateToTheBlock() < Ernest.INFINITE ) {
			isActif = true ;
		} else if ( previousPhotoreceptorState.distanceAccurateToTheBlock() < Ernest.INFINITE &&
				currentPhotoreceptorState.distanceAccurateToTheBlock() == Ernest.INFINITE ) {
			isActif = true ;
		}

		return isActif ;
	}

	@Override
	protected Eyes clone() throws CloneNotSupportedException {
		Eyes object = (Eyes) super.clone() ;
		object.photoreceptor = this.photoreceptor.clone() ;
		object.previousPhotoreceptorState = this.previousPhotoreceptorState.clone() ;
		return object ;
	}
}
