package ideal.vacuum.agent;

import java.util.EventListener ;

/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public interface GraphicPropertiesListener extends EventListener{

	public void notifyGraphicPropertiesChanged( GraphicPropertiesChangeEvent properties );
}
