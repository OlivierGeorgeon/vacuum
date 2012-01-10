package agent;
import java.util.ArrayList;

/**
 * The class that runs the simulation in a separate thread.
 */
public class ErnestView implements Runnable 
{
	private Main mainFrame;
	private boolean work=true;
	
	private Environment m_env;
	
	public ErnestView(Environment environment, Main frame)
	{
		mainFrame=frame;
		m_env = environment;
	}

	public void close()
	{
		work=false;
	}
	
	/**
	 * Run the simulation.
	 */
	public void run()
	{
		// Initialize the agents ===
		m_env.initAgents();
	
		// Run the simulation in an infinite loop ===
	
		while (work)
		{
			boolean testRun=false;
			
			mainFrame.drawGrid();
			
			m_env.update();

			if (testRun)
				try { Thread.sleep(500);
				} catch (InterruptedException e) {e.printStackTrace();}

			if (mainFrame.version!=100){
				try { Thread.sleep(20);
				} catch (InterruptedException e) {e.printStackTrace();}
			}
		}
	}	
}
