

import ernest.*;

/**
 * The main class that instantiates an Ernest agent and runs it in the environment
 * @author ogeorgeon
 */
public class ErnestView implements IView 
{
	private final ErnestModel m_model;

	public ErnestView(ErnestModel m)
	{
		m_model = m;		
	}
	public void init() 
	{
		m_model.resetStepCount();
		m_model.resetScore();
		m_model.haltAgent();
	}

	/**
	 * Run Ernest.
	 */
	public void run() 
	{
		m_model.startAgent();

		// Initialize an Ernest agent ===
		
		m_model.initErnest();
				
		// Run Ernest an infinite loop ===
		
		boolean status = false;
		
		while (!m_model.isAgentStopped())
		{
			String intention = m_model.stepErnest(status);
			status = handleAction(intention);
		}
		m_model.closeErnest();
	}

	/**
	 * Handles Ernest's actions in the grid
	 * @return the binary feedback
	 */
	protected boolean handleAction(String schema) 
	{
		return  m_model.enactSchema(schema);
	}
	
	public void setAllowState(boolean allow) 
	{
	}

	public void setRadarSensor(boolean enable) 
	{
	}
}
