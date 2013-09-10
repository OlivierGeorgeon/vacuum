package ideal.vacuum.agent ;

import javax.vecmath.Vector3f ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class GraphicProperties implements Cloneable{
	private Vector3f mPosition ;
	private Vector3f mOrientation ;
	private Vector3f mTranslation ;
	private Vector3f mRotation ;
	private Vector3f mPreviousPosition ;
	private Vector3f mPreviousOrientation ;
	private int displayCode;

	public GraphicProperties( Vector3f mPosition ,
			Vector3f mOrientation ,
			Vector3f mTranslation ,
			Vector3f mRotation ,
			Vector3f mPreviousPosition ,
			Vector3f mPreviousOrientation,
			int displayCode) {
		this.mPosition = mPosition ;
		this.mOrientation = mOrientation ;
		this.mTranslation = mTranslation ;
		this.mRotation = mRotation ;
		this.mPreviousPosition = mPreviousPosition ;
		this.mPreviousOrientation = mPreviousOrientation ;
		this.displayCode = displayCode;
	}

	@Override
	protected GraphicProperties clone() throws CloneNotSupportedException {
		GraphicProperties object = (GraphicProperties) super.clone();
		object.mPosition = (Vector3f) this.mPosition.clone();
		object.mOrientation = (Vector3f) this.mOrientation.clone();
		object.mTranslation = (Vector3f) this.mTranslation.clone();
		object.mRotation = (Vector3f) this.mRotation.clone();
		object.mPreviousPosition = (Vector3f) this.mPreviousPosition.clone();
		object.mPreviousOrientation = (Vector3f) this.mPreviousOrientation.clone();
		
		return object;
	}
	
	public Vector3f getmPosition() {
		return this.mPosition ;
	}

	public Vector3f getmOrientation() {
		return this.mOrientation ;
	}

	public Vector3f getmTranslation() {
		return this.mTranslation ;
	}

	public Vector3f getmRotation() {
		return this.mRotation ;
	}

	public Vector3f getmPreviousPosition() {
		return this.mPreviousPosition ;
	}

	public Vector3f getmPreviousOrientation() {
		return this.mPreviousOrientation ;
	}

	public void setmPosition( Vector3f mPosition ) {
		this.mPosition = mPosition ;
	}

	public void setmOrientation( Vector3f mOrientation ) {
		this.mOrientation = mOrientation ;
	}

	public void setmTranslation( Vector3f mTranslation ) {
		this.mTranslation = mTranslation ;
	}

	public void setmRotation( Vector3f mRotation ) {
		this.mRotation = mRotation ;
	}

	public void setmPreviousPosition( Vector3f mPreviousPosition ) {
		this.mPreviousPosition = mPreviousPosition ;
	}

	public void setmPreviousOrientation( Vector3f mPreviousOrientation ) {
		this.mPreviousOrientation = mPreviousOrientation ;
	}

	public int getDisplayCode() {
		return displayCode;
	}

	public void setDisplayCode(int displayCode) {
		this.displayCode = displayCode;
	}
}