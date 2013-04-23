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

	public static int RESOLUTION_RETINA = 2;
	public static final float DISTANCE_VISION = 4;
	
	public enum ActifEye {
		NONE ,
		LEFT ,
		RIGHT ,
		BOTH ;
	}

	private Eye rightEye ;
	private Eye leftEye ;
	private Eye previousRightEyeState ;
	private Eye previousLeftEyeState ;

	public Eyes() {
		this.rightEye = new Eye( Ernest.INFINITE , Ernest.INFINITE , Environment.empty.color ) ;
		this.leftEye = new Eye( Ernest.INFINITE , Ernest.INFINITE , Environment.empty.color ) ;
		this.previousRightEyeState = this.rightEye ;
		this.previousLeftEyeState = this.leftEye ;
	}

	public void updateRightEye( int xToTheblock , int yToTheblock , Color lookedBlock ) {
		this.previousRightEyeState = this.rightEye ;
		this.rightEye = new Eye( xToTheblock , yToTheblock , lookedBlock ) ;
	}

	public void updateLeftEye( int xToTheblock , int yToTheblock , Color lookedBlock ) {
		this.previousLeftEyeState = this.leftEye ;
		this.leftEye = new Eye( xToTheblock , yToTheblock , lookedBlock ) ;
	}

	public Color getLeftEyeLookedBlock() {
		return this.leftEye.getLookedBlock() ;
	}

	public double getLeftEyeDistanceToTheblock() {
		return this.leftEye.distanceToTheBlock() ;
	}

	public double getLeftEyeDistanceAccurateToTheblock() {
		return this.leftEye.distanceAccurateToTheBlock() ;
	}
	
	public Color getRightEyeLookedBlock() {
		return this.rightEye.getLookedBlock() ;
	}

	public double getRightEyeDistanceToTheblock() {
		return this.rightEye.distanceToTheBlock() ;
	}

	public double getRightEyeDistanceAccurateToTheblock() {
		return this.rightEye.distanceAccurateToTheBlock() ;
	}
	
	public int getLeftxToTheBlock() {
		return this.leftEye.getxToTheBlock() ;
	}

	public int getLeftyToTheBlock() {
		return this.leftEye.getyToTheBlock() ;
	}

	public int getRightxToTheBlock() {
		return this.rightEye.getxToTheBlock() ;
	}

	public int getRightyToTheBlock() {
		return this.rightEye.getyToTheBlock() ;
	}

	public Eyes takeSnapshot() {
		try {
			return this.clone() ;
		} catch ( CloneNotSupportedException e ) {
			e.printStackTrace() ;
		}
		return null ;
	}

	private boolean isActif( Eye previousEyeState , Eye currentEyeState ) {
		boolean isActif = false ;

		if ( previousEyeState.distanceAccurateToTheBlock() == currentEyeState.distanceAccurateToTheBlock() ) {
			isActif = false ;
		} else if ( previousEyeState.distanceAccurateToTheBlock() < Ernest.INFINITE &&
				currentEyeState.distanceAccurateToTheBlock() < previousEyeState.distanceAccurateToTheBlock() ) {
			isActif = true ;
		} else if ( previousEyeState.distanceAccurateToTheBlock() == Ernest.INFINITE &&
				currentEyeState.distanceAccurateToTheBlock() < Ernest.INFINITE ) {
			isActif = true ;
		} else if ( previousEyeState.distanceAccurateToTheBlock() < Ernest.INFINITE &&
				currentEyeState.distanceAccurateToTheBlock() == Ernest.INFINITE ) {
			isActif = true ;
		}

		return isActif ;
	}

	public Eyes.ActifEye getActifEye() {
		if ( this.isActif( this.previousLeftEyeState , this.leftEye ) &&
				this.isActif( this.previousRightEyeState , this.rightEye ) ) {
			return Eyes.ActifEye.BOTH ;
		}
		if ( this.isActif( this.previousLeftEyeState , this.leftEye ) &&
				!this.isActif( this.previousRightEyeState , this.rightEye ) ) {
			return Eyes.ActifEye.LEFT ;
		}
		if ( !this.isActif( this.previousLeftEyeState , this.leftEye ) &&
				this.isActif( this.previousRightEyeState , this.rightEye ) ) {
			return Eyes.ActifEye.RIGHT ;
		}
		return Eyes.ActifEye.NONE ;
	}

	@Override
	protected Eyes clone() throws CloneNotSupportedException {
		Eyes object = (Eyes) super.clone() ;
		object.leftEye = this.leftEye.clone() ;
		object.rightEye = this.rightEye.clone() ;
		return object ;
	}

	private class Eye implements Cloneable {

		private Color lookedBlock ;
		private int xToTheBlock ;
		private int yToTheBlock ;

		public Eye( int xToTheblock , int yToTheblock , Color lookedBlock ) {
			this.xToTheBlock = xToTheblock ;
			this.yToTheBlock = yToTheblock ;
			this.lookedBlock = lookedBlock ;
		}

		public Color getLookedBlock() {
			return this.lookedBlock ;
		}

		public int getxToTheBlock() {
			return this.xToTheBlock ;
		}

		public int getyToTheBlock() {
			return this.yToTheBlock ;
		}

		public double distanceAccurateToTheBlock() {
			if ( Math.abs( this.xToTheBlock ) == Ernest.INFINITE ||
					Math.abs( this.yToTheBlock ) == Ernest.INFINITE )
				return Ernest.INFINITE ;
			return this.distanceToTheBlock() * Ernest.INT_FACTOR ;
		}

		public double distanceToTheBlock() {
			return this.calculateHypotenuse();
		}
		
		private double calculateHypotenuse() {
			return ( Math.sqrt( this.xToTheBlock * this.xToTheBlock + this.yToTheBlock *
					this.yToTheBlock ) ) ;
		}

		@Override
		public Eye clone() throws CloneNotSupportedException {
			Eye object = (Eye) super.clone() ;
			object.xToTheBlock = this.xToTheBlock ;
			object.yToTheBlock = this.yToTheBlock ;
			object.lookedBlock = this.lookedBlock ;
			return object ;
		}
	}
}
