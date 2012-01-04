package agent;
import java.util.ArrayList;



/**
 * The main class that instantiates an Ernest agent and runs it in the environment
 * @author ogeorgeon
 */
public class ErnestView implements Runnable//implements IView 
{
	private final ArrayList<ErnestModel> m_modelList;
	private Main mainFrame;
	private boolean run=false;
	private boolean work=true;
	
	public ErnestView(ArrayList<ErnestModel> m, Main frame)
	{
		m_modelList=m;
		mainFrame=frame;
	}
//	public void init() 
//	{
//		m_model.haltAgent();
//	}
	
	public void runSimulation(){
		run=true;
	}
	
	public void stop(){
		run=false;
	}

	public void close(){
		work=false;
	}

	
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
	
		while (work){
			boolean testRun=true;
			
			mainFrame.drawGrid();
			
			for (int i=0;i<m_modelList.size();i++){
				if (m_modelList.get(i).run || m_modelList.get(i).step){
					m_modelList.get(i).update();
					testRun=false;
					
					if (!m_modelList.get(i).isStep) m_modelList.get(i).step=false;
				}
				
				

			}
			if (testRun)
				try { Thread.sleep(500);
				} catch (InterruptedException e) {e.printStackTrace();}

			try { Thread.sleep(20);
            } catch (InterruptedException e) {e.printStackTrace();}

			
		}

		for (int i=0;i<m_modelList.size();i++){
			m_modelList.get(i).closeErnest();
		}

	}	
}
