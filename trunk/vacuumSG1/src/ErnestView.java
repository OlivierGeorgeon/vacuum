

/**
 * The main class that instantiates an Ernest agent and runs it in the environment
 * @author ogeorgeon
 */
public class ErnestView implements Runnable//implements IView 
{
	private final ErnestModel m_model;

	public ErnestView(ErnestModel m)
	{
		m_model = m;		
	}
//	public void init() 
//	{
//		m_model.haltAgent();
//	}

	/**
	 * Run Ernest.
	 */
	public void run() 
	{
		m_model.startAgent();

		// Initialize an Ernest agent ===
		
		m_model.initErnest();
				
		// Run Ernest an infinite loop ===
		
		boolean status = true; // not bump on step 0.
		
		while (!m_model.isAgentStopped())
		{
			String intention = m_model.stepErnest(status);
			status = m_model.enactSchema(intention);
		}
		m_model.closeErnest();
	}	
}
