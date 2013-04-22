package ideal.vacuum;

public interface FramePlugin{

	public void refresh();
	
	public void anim( float angleRotation, float xTranslation );

	public void close() ;

	public void display() ;
}
