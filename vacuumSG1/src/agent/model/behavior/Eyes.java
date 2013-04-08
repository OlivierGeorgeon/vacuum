package agent.model.behavior;

import java.awt.Color ;

import ernest.Ernest ;

import agent.Environment ;

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
	
	public Eyes() {
		this.rightEye = new Eye( Environment.empty.color , Ernest.INFINITE );
		this.leftEye = new Eye( Environment.empty.color , Ernest.INFINITE );
	}
	
	public void updateRightEye( Color lookedBlock , int distanceToTheblock ){
		this.rightEye = new Eye( lookedBlock , distanceToTheblock );
	}
	
	public void updateLeftEye( Color lookedBlock , int distanceToTheblock ){
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
	
	public Eyes.ActifEye getActifEye() {
		if ( this.leftEye.distanceToTheblock == Ernest.INFINITE && this.rightEye.distanceToTheblock == Ernest.INFINITE ) {
			return Eyes.ActifEye.NONE;
		}
		if ( this.leftEye.distanceToTheblock != Ernest.INFINITE && this.rightEye.distanceToTheblock == Ernest.INFINITE ) {
			return Eyes.ActifEye.LEFT;
		}
		if ( this.leftEye.distanceToTheblock == Ernest.INFINITE && this.rightEye.distanceToTheblock != Ernest.INFINITE ) {
			return Eyes.ActifEye.RIGHT;
		}
		return Eyes.ActifEye.BOTH;
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
