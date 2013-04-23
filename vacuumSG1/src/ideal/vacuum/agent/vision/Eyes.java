package ideal.vacuum.agent.vision ;

import ideal.vacuum.Environment ;

import java.awt.Color ;

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
		return this.previousPhotoreceptorState.getxBlockPosition() ;
	}

	public int getyBlockPosition() {
		return this.previousPhotoreceptorState.getyBlockPosition() ;
	}

	public Color getBlockColor() {
		return this.previousPhotoreceptorState.getBlockColor() ;
	}

	public double distanceAccurateToTheBlock() {
		return this.previousPhotoreceptorState.distanceAccurateToTheBlock() ;
	}

	public double distanceToTheBlock() {
		return this.previousPhotoreceptorState.distanceToTheBlock() ;
	}

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
