package ideal.vacuum.agent.behavior;

import ideal.vacuum.Environment ;

import java.awt.Color ;

import ernest.Ernest ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class Eyes implements Cloneable{

	public enum ActifEye{
		NONE,
		LEFT,
		RIGHT,
		BOTH;
	}
	
	private Eye rightEye;
	private Eye leftEye;
	private Eye previousRightEyeState;
	private Eye previousLeftEyeState;
	
	public Eyes() {
		this.rightEye = new Eye( Environment.empty.color , Ernest.INFINITE );
		this.leftEye = new Eye( Environment.empty.color , Ernest.INFINITE );
		this.previousRightEyeState = this.rightEye;
		this.previousLeftEyeState = this.leftEye;
	}
	
	public void updateRightEye( Color lookedBlock , int distanceToTheblock ){
		this.previousRightEyeState = this.rightEye;
		this.rightEye = new Eye( lookedBlock , distanceToTheblock );
	}
	
	public void updateLeftEye( Color lookedBlock , int distanceToTheblock ){
		this.previousLeftEyeState = this.leftEye;
		this.leftEye = new Eye( lookedBlock , distanceToTheblock );
	}
	
	public Color getLeftEyeLookedBlock() {
		return this.leftEye.getLookedBlock() ;
	}

	public int getLeftEyeDistanceToTheblock() {
		return this.leftEye.getDistanceToTheblock() ;
	}

	public Color getRightEyeLookedBlock() {
		return this.rightEye.getLookedBlock() ;
	}

	public int getRightEyeDistanceToTheblock() {
		return this.rightEye.getDistanceToTheblock() ;
	}

	public Eyes takeSnapshot(){
		try {
			return this.clone();
		}
		catch ( CloneNotSupportedException e ) {
			e.printStackTrace();
		}
		return null ;
	}
	
	private boolean isActif( Eye previousEyeState , Eye currentEyeState ){
		boolean isActif = false;
		
		if ( previousEyeState.distanceToTheblock == currentEyeState.distanceToTheblock ) {
			isActif = false ;
		}else if ( previousEyeState.distanceToTheblock < Ernest.INFINITE && currentEyeState.distanceToTheblock < previousEyeState.distanceToTheblock ) {
			isActif = true ;
		}else if ( previousEyeState.distanceToTheblock == Ernest.INFINITE && currentEyeState.distanceToTheblock < Ernest.INFINITE ) {
			isActif = true ;
		}else if ( previousEyeState.distanceToTheblock < Ernest.INFINITE && currentEyeState.distanceToTheblock == Ernest.INFINITE ) {
			isActif = false ;
		}

		return isActif ;
	}
	public Eyes.ActifEye getActifEye() {
		if ( this.isActif( this.previousLeftEyeState , this.leftEye ) && this.isActif( this.previousRightEyeState , this.rightEye )) {
			return Eyes.ActifEye.BOTH;
		}
		if ( this.isActif( this.previousLeftEyeState , this.leftEye ) && ! this.isActif( this.previousRightEyeState , this.rightEye )) {
			return Eyes.ActifEye.LEFT;
		}
		if ( ! this.isActif( this.previousLeftEyeState , this.leftEye ) && this.isActif( this.previousRightEyeState , this.rightEye )) {
			return Eyes.ActifEye.RIGHT;
		}
		return Eyes.ActifEye.NONE;
	}
	
	@Override
	protected Eyes clone() throws CloneNotSupportedException {
		Eyes object = (Eyes) super.clone();
		object.leftEye = this.leftEye.clone();
		object.rightEye = this.rightEye.clone();
		return object ;
	}
	
	private class Eye implements Cloneable {
		
		private Color lookedBlock;
		private int distanceToTheblock;
		
		public Eye( Color lookedBlock , int distanceToTheblock ) {
			this.lookedBlock = lookedBlock;
			this.distanceToTheblock = distanceToTheblock;
		}
		
		public Color getLookedBlock() {
			return this.lookedBlock ;
		}
		
		public int getDistanceToTheblock() {
			return this.distanceToTheblock ;
		}
		
		@Override
		public Eye clone() throws CloneNotSupportedException {
			Eye object = (Eye) super.clone() ;
			object.distanceToTheblock = this.distanceToTheblock;
			object.lookedBlock = this.lookedBlock;
			return object ;
		}
	}
}
