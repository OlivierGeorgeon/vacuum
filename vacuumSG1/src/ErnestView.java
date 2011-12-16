import java.util.ArrayList;



/**
 * The main class that instantiates an Ernest agent and runs it in the environment
 * @author ogeorgeon
 */
public class ErnestView implements Runnable//implements IView 
{
	private final ArrayList<ErnestModel> m_modelList;
	
	public ErnestView(ArrayList<ErnestModel> m)
	{
		m_modelList=m;
	}
//	public void init() 
//	{
//		m_model.haltAgent();
//	}

	/**
	 * Run Ernest.
	 */
	public void run(){

		
		// Initialize an Ernest agent ===
		for (int i=0;i<m_modelList.size();i++){
			m_modelList.get(i).startAgent();
			m_modelList.get(i).initErnest();
		}
	
		// Run Ernest an infinite loop ===
	
	
		while (!m_modelList.get(0).isAgentStopped()){
			for (int i=0;i<m_modelList.size();i++){
				m_modelList.get(i).stepAgent();
			}
		}

		for (int i=0;i<m_modelList.size();i++){
			m_modelList.get(i).closeErnest();
		}

	}	
}
