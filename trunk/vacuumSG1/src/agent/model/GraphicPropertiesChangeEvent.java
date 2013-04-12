package agent.model ;

import java.util.EventObject ;

import javax.vecmath.Vector3f ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public class GraphicPropertiesChangeEvent extends EventObject {

	private GraphicProperties graphicProperties ;

	public GraphicPropertiesChangeEvent( Object source ,
			GraphicProperties graphicProperties ) {
		super( source ) ;
		try {
			this.graphicProperties = graphicProperties.clone() ;
		} catch ( CloneNotSupportedException e ) {
			e.printStackTrace() ;
		}
	}

	public GraphicProperties getGraphicProperties() {
		return this.graphicProperties ;
	}

	
	public Vector3f getmPosition() {
		return this.graphicProperties.getmPosition() ;
	}

	public Vector3f getmOrientation() {
		return this.graphicProperties.getmOrientation() ;
	}

	public Vector3f getmTranslation() {
		return this.graphicProperties.getmTranslation() ;
	}

	public Vector3f getmRotation() {
		return this.graphicProperties.getmRotation() ;
	}

	public Vector3f getmPreviousPosition() {
		return this.graphicProperties.getmPreviousPosition() ;
	}

	public Vector3f getmPreviousOrientation() {
		return this.graphicProperties.getmPreviousOrientation() ;
	}

	public void setmPosition( Vector3f mPosition ) {
		this.graphicProperties.setmPosition( mPosition ) ;
	}

	public void setmOrientation( Vector3f mOrientation ) {
		this.graphicProperties.setmOrientation( mOrientation ) ;
	}

	public void setmTranslation( Vector3f mTranslation ) {
		this.graphicProperties.setmTranslation( mTranslation ) ;
	}

	public void setmRotation( Vector3f mRotation ) {
		this.graphicProperties.setmRotation( mRotation ) ;
	}

	public void setmPreviousPosition( Vector3f mPreviousPosition ) {
		this.graphicProperties.setmPreviousPosition( mPreviousPosition ) ;
	}

	public void setmPreviousOrientation( Vector3f mPreviousOrientation ) {
		this.graphicProperties.setmPreviousOrientation( mPreviousOrientation ) ;
	}
}