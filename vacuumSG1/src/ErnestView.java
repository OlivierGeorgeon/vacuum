import java.util.ArrayList;



/**
 * The main class that instantiates an Ernest agent and runs it in the environment
 * @author ogeorgeon
 */
public class ErnestView implements Runnable//implements IView 
{
	private final ArrayList<ErnestModel> m_modelList;
	private final ErnestModel m_model;
	
	public ErnestView(ErnestModel m)
	{
		m_model = m;
		m_modelList=new ArrayList<ErnestModel>();
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
		m_model.startAgent();
		m_model.initErnest();
	
		// Run Ernest an infinite loop ===
	
	
		while (!m_model.isAgentStopped()){
			m_model.stepAgent();
		}

		m_model.closeErnest();
		
		//======================================================
		
		if (m_modelList.size()>0){
			// Initialize an Ernest agent ===
			for (int i=0;i<m_modelList.size();i++){
				m_modelList.get(i).startAgent();
				m_modelList.get(i).initErnest();
			}
		
			// Run Ernest an infinite loop ===
		
		
			while (!m_modelList.get(0).isAgentStopped()){
				for (int i=0;i<m_modelList.size();i++){
					m_modelList.get(0).stepAgent();
				}
			}

			for (int i=0;i<m_modelList.size();i++){
				m_modelList.get(i).closeErnest();
			}
		}
		else
			System.out.println("no agent in the environment");
		//====================================================
	}	
}
