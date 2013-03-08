package agent.model;

import java.util.EventListener ;

public interface GraphicPropertiesListener extends EventListener{

	public void notifyGraphicPropertiesChanged( GraphicPropertiesChangeEvent properties );
}
