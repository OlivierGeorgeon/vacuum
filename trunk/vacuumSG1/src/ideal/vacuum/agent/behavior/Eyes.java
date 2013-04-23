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
		this.rightEye = new Eye(Ernest.INFINITE, Ernest.INFINITE, Environment.empty.color );
		this.leftEye = new Eye(Ernest.INFINITE, Ernest.INFINITE, Environment.empty.color);
		this.previousRightEyeState = this.rightEye;
		this.previousLeftEyeState = this.leftEye;
	}
	
	public void updateRightEye( int x, int y, Color lookedBlock ){
		this.previousRightEyeState = this.rightEye;
		this.rightEye = new Eye( x, y, lookedBlock);
	}
	
	public void updateLeftEye( int x , int y, Color lookedBlock ){
		this.previousLeftEyeState = this.leftEye;
		this.leftEye = new Eye(x, y,  lookedBlock );
	}
	
	public Color getLeftEyeLookedBlock() {
		return this.leftEye.getLookedBlock() ;
	}

	public int getLeftEyeDistanceToTheblock() {
		return this.leftEye.distanceToTheblock() ;
	}

	public Color getRightEyeLookedBlock() {
		return this.rightEye.getLookedBlock() ;
	}

	public int getRightEyeDistanceToTheblock() {
		return this.rightEye.distanceToTheblock() ;
	}
	
	

	public int getLeftxToTheblock() {
		return leftEye.getxToTheblock();
	}

	public int getLeftyToTheblock() {
		return leftEye.getyToTheblock();
	}

	public int getRightxToTheblock() {
		return rightEye.getxToTheblock();
	}

	public int getRightyToTheblock() {
		return rightEye.getyToTheblock();
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
		
		if ( previousEyeState.distanceToTheblock() == currentEyeState.distanceToTheblock() ) {
			isActif = false ;
		}else if ( previousEyeState.distanceToTheblock() < Ernest.INFINITE && currentEyeState.distanceToTheblock() < previousEyeState.distanceToTheblock() ) {
			isActif = true ;
		}else if ( previousEyeState.distanceToTheblock() == Ernest.INFINITE && currentEyeState.distanceToTheblock() < Ernest.INFINITE ) {
			isActif = true ;
		}else if ( previousEyeState.distanceToTheblock() < Ernest.INFINITE && currentEyeState.distanceToTheblock() == Ernest.INFINITE ) {
			isActif = true ;
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
		private int xToTheblock;
		private int yToTheblock;
		
		public Eye( int x , int y , Color lookedBlock ) {
			this.xToTheblock = x;
			this.yToTheblock = y;
			this.lookedBlock = lookedBlock;
		}
		
		public Color getLookedBlock() {
			return this.lookedBlock ;
		}
		
		public int getxToTheblock() {
			return xToTheblock;
		}
		
		public int getyToTheblock() {
			return yToTheblock;
		}
		
		public int distanceToTheblock()
		{
			if (Math.abs(xToTheblock) == Ernest.INFINITE || Math.abs(yToTheblock) == Ernest.INFINITE )
				return Ernest.INFINITE;
			return (int)(Math.sqrt((this.xToTheblock * this.xToTheblock  + this.yToTheblock * this.yToTheblock) * Ernest.INT_FACTOR * Ernest.INT_FACTOR));
		}
		
		@Override
		public Eye clone() throws CloneNotSupportedException {
			Eye object = (Eye) super.clone() ;
			object.xToTheblock = this.xToTheblock;
			object.yToTheblock = this.yToTheblock;
			object.lookedBlock = this.lookedBlock;
			return object ;
		}
	}
}
