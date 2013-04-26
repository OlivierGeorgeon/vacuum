package ideal.vacuum.agent.vision ;

import ideal.vacuum.Environment ;
import ideal.vacuum.agent.VisualEffect ;

import java.awt.Color ;

import javax.vecmath.Point3f ;

import utils.ErnestUtils;

import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class Eyes implements Cloneable {

	public static int RESOLUTION_RETINA = 1 ;
	public static final float DISTANCE_VISION = 4 ;

	private PhotoreceptorCell photoreceptor ;
	private PhotoreceptorCell previousPhotoreceptorState ;

	public Eyes() {
		this.photoreceptor = new PhotoreceptorCell(
				Ernest.INFINITE ,
				Ernest.INFINITE ,
				Environment.empty.color ) ;
		this.previousPhotoreceptorState = this.photoreceptor ;
	}

	public void updateEye( int xBlockPosition , int yBlockPosition , Color blockColor ) {
		this.previousPhotoreceptorState = this.photoreceptor ;
		this.photoreceptor = new PhotoreceptorCell( xBlockPosition , yBlockPosition , blockColor ) ;
	}

	public int getxBlockPosition() {
		return this.photoreceptor.getxBlockPosition() ;
	}

	public int getyBlockPosition() {
		return this.photoreceptor.getyBlockPosition() ;
	}

	public Color getBlockColor() {
		return this.photoreceptor.getBlockColor() ;
	}

	public float distanceAccurateToTheBlock() {
		return this.photoreceptor.distanceAccurateToTheBlock() ;
	}

	public Eyes takeSnapshot() {
		try {
			return this.clone() ;
		} catch ( CloneNotSupportedException e ) {
			e.printStackTrace() ;
		}
		return null ;
	}

	public VisualEffect visualEffect() {
		VisualEffect stimuli = VisualEffect.UNCHANGED ;
		float previousDistance = this.previousPhotoreceptorState.distanceAccurateToTheBlock() ;
		float currentDistance = this.photoreceptor.distanceAccurateToTheBlock() ;

		if ( Math.abs(previousDistance - currentDistance ) < .1) {
			if (currentDistance == Ernest.INFINITE)
				stimuli = VisualEffect.UNCHANGED ;
			else if (Math.abs(ErnestUtils.polarAngle(this.photoreceptor.getBlockPosition())) < .1f)
				stimuli = VisualEffect.CLOSER;
			else
				stimuli = VisualEffect.MOVE ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance < previousDistance ) {
			stimuli = VisualEffect.CLOSER ;
		} else if ( previousDistance == Ernest.INFINITE && currentDistance < Ernest.INFINITE ) {
			stimuli = VisualEffect.APPEAR ;
		} else if ( previousDistance < Ernest.INFINITE && currentDistance == Ernest.INFINITE ) {
			stimuli = VisualEffect.DISAPPEAR ;
		}

		return stimuli ;
	}

	public Point3f getEventPosition() {
		float d = 0 ;
		Point3f position = new Point3f() ;
		VisualEffect stimuli = this.visualEffect() ;

		switch ( stimuli ) {
			case CLOSER:
			case APPEAR:
			case MOVE:
				position = this.photoreceptor.getBlockPosition() ;
				d = this.photoreceptor.distanceAccurateToTheBlock() ;
				if ( d > 0 && d < Ernest.INFINITE )
					position.scale( DISTANCE_VISION / d ) ;
				break ;
			case DISAPPEAR:
				position = this.previousPhotoreceptorState.getBlockPosition() ;
				d = this.previousPhotoreceptorState.distanceAccurateToTheBlock() ;
				if ( d > 0 && d < Ernest.INFINITE )
					position.scale( DISTANCE_VISION / d ) ;
				break ;
		}

		return position ;
	}

	@Override
	public Eyes clone() throws CloneNotSupportedException {
		Eyes object = (Eyes) super.clone() ;
		object.photoreceptor = this.photoreceptor.clone() ;
		object.previousPhotoreceptorState = this.previousPhotoreceptorState.clone() ;
		return object ;
	}
}
