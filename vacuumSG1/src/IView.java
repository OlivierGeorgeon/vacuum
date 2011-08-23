

import java.util.*;
import javax.swing.JOptionPane;


/**
 * Interface for an agent thread  
 * @author ogeorgeon Intended to represent any view: Soar or Jess or java or Human
 * So far only represents Soar 
 */
public interface IView extends Runnable
{

    public void run();

    public void init();

	public void setAllowState(boolean bAllow);
	
	public void setRadarSensor(boolean bEnable);
	
}
