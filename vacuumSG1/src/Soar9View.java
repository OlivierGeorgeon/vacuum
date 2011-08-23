

import java.util.*;
import java.util.regex.*;
import javax.swing.JOptionPane;
import jess.JessException;
import sml.*;


/**
 * Soar 9 Execution  
 * @author mcohen
 * @author ogeorgeon adapt to Soar 9 
 *         Updates innerAgentPanel
 */
public class Soar9View implements IView, Agent.RunEventInterface, Agent.PrintEventInterface, Kernel.RhsFunctionInterface
{
	private static Random m_rand = new Random();
	
	private static final String SPOT_FACT_NAME = "vacuum.types.spot";
	private static final String POS_FACT_NAME = "vacuum.types.position";
	private static final String ACTION_FACT_NAME = "|vacuum.types.action|";
	private static final String RADAR_FACT_NAME = "vacuum.types.radar";
	private static final String VISION_FACT_NAME = "vacuum.types.vision";
	
	private Identifier m_spotFact;
	private Identifier m_posFact;
	private Identifier m_leftRadarFact;		
	private Identifier m_rightRadarFact;
	private Identifier m_upRadarFact;
	private Identifier m_downRadarFact;	
	private Identifier m_visionFact;		

	private final ErnestModel m_model;
	private boolean m_bAllowState = true;
	private boolean m_bRadarSensor = true;
	private boolean m_bStatus = false;
	
    private Kernel m_kernel;
    private Agent m_agent;
	
	private LogTraceXml soarLog;

	public Soar9View(ErnestModel m)
	{
		m_model = m;
	}	

	public void init()
	{
		m_model.resetStepCount();
		m_model.resetScore();
		m_model.haltAgent();
	}

	public void setAllowState(boolean bAllow)
	{ m_bAllowState = bAllow; }
	
	public boolean getAllowState()
	{ return m_bAllowState; }

	public void setRadarSensor(boolean bEnable)
	{ m_bRadarSensor = bEnable; }
	
	public boolean getRadarSensor()
	{ return m_bRadarSensor; }

	/**
	 * Run a Soar agent 
	 * @author mcohen
	 * @author ogeorgeon adapt rhsFunction to Soar 9 
	 * 		   log activity trace
	 */
	public void run()
	{
		m_model.startAgent();
		soarLog = new LogTraceXml("trace.xml");

		try
		{
	        try 
	        {
	            m_kernel = Kernel.CreateKernelInNewThread("SoarKernelSML");
	        }
	        catch (Exception e)
	        {
				JOptionPane.showMessageDialog(Main.MAIN_WIN, 
						"Error loading the Soar engine!\n" + 
						"Please restart the environment with ElementXML.dll, SoarKernelSML.dll, and Java_sml_ClientInterface.dll\n" + 
						"included in current directory or the system path.\n" + 
						"(NOTE: these files are included in the Soar distribution)",
						"Error!", 
						JOptionPane.ERROR_MESSAGE);
				return;
	        }
	        
	        if (m_kernel.HadError())
	        {
	        	System.out.println("Error creating kernel: " + m_kernel.GetLastErrorDescription()) ;
	            System.exit(1);
	        }
			String SmlVersion = m_kernel.GetSoarKernelVersion() ;
			System.out.println("Soar version " + SmlVersion) ;
	        
			// Olivier
	        m_kernel.AddRhsFunction("rand", this, this);
	        // m_kernel.AddRhsFunction("rand", this, "rand", this);
	        
	        m_agent = m_kernel.CreateAgent("vacuumAgent");
	        boolean load = m_agent.LoadProductions(m_model.getAgentFile());
	        if (!load || m_agent.HadError()) 
	        {
	            throw new IllegalStateException("Error loading productions: "
	                    + m_agent.GetLastErrorDescription());
	        }

	        // Register for the event we'll use to update the world
	        registerForUpdateWorldEvent() ;

	        initFacts();
			
			m_kernel.RunAllAgentsForever(); // Thread stays on this instruction until it is halted 
			
			soarLog.logLine("</xml>");

			//JOptionPane.showMessageDialog(Main.MAIN_WIN, 
			//	"Final Score: " + m_model.getScore(),
			//	"Score", 
			//	JOptionPane.INFORMATION_MESSAGE);
		}
		catch (Exception e)
		{
			JOptionPane.showMessageDialog(Main.MAIN_WIN, 
					e.toString(),
					"Soar Error", 
					JOptionPane.ERROR_MESSAGE);
			System.out.println(e);
		}
		finally
		{
			// clean up no matter what happens...
			init(); 
			
			if (m_kernel != null)
			{
				m_kernel.DestroyAgent(m_agent);
	    		m_kernel.Shutdown();
	    		m_kernel.delete();
			}
	        m_agent = null;
	        m_kernel = null;
	        m_spotFact = null;
	        m_posFact = null;
	        m_visionFact = null;
	        m_leftRadarFact = null;
	        m_rightRadarFact = null;
	        m_upRadarFact = null;
	        m_downRadarFact = null;
		}
	}

	protected void preStep()
	{
		m_model.tallyScore();
//		System.out.println("Step #" + m_model.getStepCount());
		m_model.decStepCount();
	}
	
	/**
	 * Handle actions from the model 
	 * @author mcohen
	 * @author ogeorgeon print the commands for verification
	 * @throws JessException
	 */
	protected void handleAction() throws JessException
	{
		if (m_agent.Commands())
		{
	        Identifier command = m_agent.GetCommand(0);
			System.out.println("Command: " + command.GetCommandName() ) ;			
	        if (command.GetCommandName().equals(ACTION_FACT_NAME)) 
	        {
				//System.out.println("Parameter move: " + command.GetParameterValue("move") ) ;			
	        	String action = command.GetParameterValue("move");
	        	if (action != null && action.equals("suck"))
	        		m_model.suck();
	        	else if (action.equals("up"))
	        		m_model.up();
        		else if (action.equals("down"))
        			m_model.down();
        		else if (action.equals("left"))
        			m_model.left();
        		else if (action.equals("right"))
        			m_model.right();
        		else if (action.equals("turnRight"))
        			m_bStatus = m_model.turnRight();
        		else if (action.equals("turnLeft"))
        			m_bStatus = m_model.turnLeft();
        		else if (action.equals("forward"))
        			m_bStatus = m_model.forward();
        		else if (action.equals("senseForward"))
        			m_bStatus = m_model.touchForward(true);
        		else if (action.equals("senseRight"))
        			m_bStatus = m_model.touchRight(true);
        		else if (action.equals("senseLeft"))
        			m_bStatus = m_model.touchLeft(true);

		        command.AddStatusComplete();
		        modifyFacts();
	        }
	        m_agent.ClearOutputLinkChanges();
	        
	        //m_innerAgentPanel.setMoveForward(true);
	        
		}
	}

	protected void postStep() throws JessException
	{
		if (!m_bAllowState)
		{
		}
	}
	
	protected void initFacts() throws JessException
	{
		modifyFacts();
	}
	
	/**
	 * Modify the facts
	 * @author mcohen
	 * @author ogeorgeon Returns the status value from a schema enaction
	 * @throws JessException
	 */
	protected void modifyFacts() throws JessException
	{
    	// Position facts
		if (m_spotFact != null)
    		m_agent.DestroyWME(m_spotFact);
        m_spotFact = m_agent.CreateIdWME(m_agent.GetInputLink(), SPOT_FACT_NAME);
       	m_agent.CreateStringWME(m_spotFact, "status", m_model.isDirty() ? "dirty" : "clean");

       	if (m_posFact != null)
    		m_agent.DestroyWME(m_posFact);
        m_posFact = m_agent.CreateIdWME(m_agent.GetInputLink(), POS_FACT_NAME);
        
	    int x = (int) m_model.agentX() ; int y = (int) m_model.agentY();
       	m_agent.CreateIntWME(m_posFact, "x", x);
       	m_agent.CreateIntWME(m_posFact, "y", y);
       	
       	// ogeorgeon Status value from a schema enaction
       	m_agent.CreateStringWME(m_posFact,"get", m_bStatus ? "S" : "F" );

       	// Radar facts
		if (m_bRadarSensor)
		{
			if (m_leftRadarFact != null)
			{
				m_agent.DestroyWME(m_leftRadarFact);
				m_agent.DestroyWME(m_rightRadarFact);
				m_agent.DestroyWME(m_upRadarFact);
				m_agent.DestroyWME(m_downRadarFact);
			}
			
	        m_leftRadarFact = m_agent.CreateIdWME(m_agent.GetInputLink(), RADAR_FACT_NAME);
	        m_rightRadarFact = m_agent.CreateIdWME(m_agent.GetInputLink(), RADAR_FACT_NAME);
	        m_upRadarFact = m_agent.CreateIdWME(m_agent.GetInputLink(), RADAR_FACT_NAME);
	        m_downRadarFact = m_agent.CreateIdWME(m_agent.GetInputLink(), RADAR_FACT_NAME);        
	       	
	        m_agent.CreateStringWME(m_leftRadarFact, "dir", "left");
	        m_agent.CreateStringWME(m_rightRadarFact, "dir", "right");
	        m_agent.CreateStringWME(m_upRadarFact, "dir", "up");
	        m_agent.CreateStringWME(m_downRadarFact, "dir", "down");

		    x = (int) (m_model.agentX() - 1); y = (int) m_model.agentY();
		    if ((x >= 0) && !m_model.isWall(x, y))
		        m_agent.CreateStringWME(m_leftRadarFact, "reading", m_model.isDirty(x, y) ? "dirty" : "clean");
		    else
		    	m_agent.CreateStringWME(m_leftRadarFact, "reading", "wall");	    	

		    x = (int) (m_model.agentX() + 1); y = (int) m_model.agentY();
		    if ((x < m_model.getWidth()) && !m_model.isWall(x, y))
		        m_agent.CreateStringWME(m_rightRadarFact, "reading", m_model.isDirty(x, y) ? "dirty" : "clean");
		    else
		    	m_agent.CreateStringWME(m_rightRadarFact, "reading", "wall");	    	
		    	
		    x = (int) m_model.agentX(); y = (int) (m_model.agentY() - 1);
		    if ((y >= 0) && !m_model.isWall(x, y))
		        m_agent.CreateStringWME(m_upRadarFact, "reading", m_model.isDirty(x, y) ? "dirty" : "clean");
		    else
		    	m_agent.CreateStringWME(m_upRadarFact, "reading", "wall");	    	

		    x = (int) m_model.agentX(); y = (int) (m_model.agentY() + 1);
		    if ((y < m_model.getHeight()) && !m_model.isWall(x, y))
		        m_agent.CreateStringWME(m_downRadarFact, "reading", m_model.isDirty(x, y) ? "dirty" : "clean");
		    else
		    	m_agent.CreateStringWME(m_downRadarFact, "reading", "wall");	    	

		}
		    
		// Agent vision facts
		if (m_visionFact != null) 
			m_agent.DestroyWME(m_visionFact);
		
		m_visionFact = m_agent.CreateIdWME(m_agent.GetInputLink(), VISION_FACT_NAME);
       	
        m_agent.CreateStringWME(m_visionFact, "left", m_model.touchLeft(false) ? "S" : "F");
        m_agent.CreateStringWME(m_visionFact, "forward", m_model.touchForward(false) ? "S" : "F"); 
        m_agent.CreateStringWME(m_visionFact, "right", m_model.touchRight(false) ? "S" : "F");

		m_agent.Commit();
		
	}
	
	/**
	 * Set up the event listeners 
	 * @author mcohen
	 * @author ogeorgeon adapt to Soar 9 
	 */
    public void registerForUpdateWorldEvent()
    {
    	m_agent.RegisterForRunEvent(smlRunEventId.smlEVENT_AFTER_DECISION_CYCLE, this, this) ;
		m_agent.RegisterForPrintEvent(smlPrintEventId.smlEVENT_PRINT, this, this) ;		    	
//      Soar 8
//    	m_agent.RegisterForRunEvent(smlRunEventId.smlEVENT_AFTER_DECISION_CYCLE, this, "afterDecisionHandler", null) ;
//		m_agent.RegisterForPrintEvent(smlPrintEventId.smlEVENT_PRINT, this, "printEventHandler", this) ;		    	
    }

	/**
	 * Handle the output of the write commands of the Soar Model 
	 * @author mcohen
	 * @author ogeorgeon Speaks the rational written by a soar rule
	 */
	public void printEventHandler(int eventID, Object data, Agent agent, String message)
	{
		System.out.println(message);
		
		Scanner scan = new Scanner(message);
		while (scan.hasNext())
		{
			String line = scan.nextLine();

			// Logs the trace
			
			Pattern pattern = Pattern.compile("^<(.*)>(.*)</(.*)>$");
			Matcher matcher = pattern.matcher(line);
			boolean matchFound = matcher.find();
			if ( matchFound )
			{
				soarLog.logLine(line);
			}
			
			// Speaks the rational		
			
			if (m_model.getSpeakAloud())
			{
				pattern = Pattern.compile("^<rational>(.*)</rational>$");
				matcher = pattern.matcher(line);
				matchFound = matcher.find();

				
			}
			
			// Restarts the model
			
			if ( line.indexOf("Stop") > 0 )
			{
				System.out.println("Agent self stopped");
				try
				{ 
					m_model.init(m_model.getBoardFileName()); 
				}
				catch (Exception ex)
				{
					System.out.println("Error reinitializing the board");
				}
			}
		}
	}

	/**
	 * Random function  
	 * @author mcohen
	 * @author ogeorgeon adapt to Soar 9 
	 */
	public String rhsFunctionHandler(int eventID, Object data, String agentName, String functionName, String argument)
//	public String rand(int id, Object data, String agentName, String functionName, String arguments)
	{
		List<String> choices = new ArrayList<String>();
		// StringTokenizer tokenizer = new StringTokenizer(arguments, ",");
		StringTokenizer tokenizer = new StringTokenizer(argument, ",");
		while (tokenizer.hasMoreTokens())
		{
			String tk = tokenizer.nextToken();
			if (tk.trim().length() > 0)
				choices.add(tk.trim());
		}
		
		int i = 0;
		if (choices.size() > 0)
			i = m_rand.nextInt(choices.size());

		return choices.get(i);
	}
	
	/**
	 * Handle Run Events 
	 * @author mcohen
	 * @author ogeorgeon adapt to Soar 9 
	 */
	public void runEventHandler(int eventID, Object data, Agent agent, int phase)
//	public void afterDecisionHandler(int eventID, Object data, Agent agent, int phase)
	{
		try
		{
			if ((m_model.getStepCount() > 0) && !m_model.isAgentStopped()) 
			{
				preStep();
				
				//dump working memory here...
		        String res = m_kernel.ExecuteCommandLine("print s1 -d 10", m_agent.GetAgentName());
		        System.out.println("* " + res);
		        res = m_kernel.ExecuteCommandLine("print s2 -d 10", m_agent.GetAgentName());
		        //System.out.println("* " + res);
				
				handleAction();
				postStep();

				try 
				{ Thread.sleep(m_model.getDelay()); }
				catch (InterruptedException ue)
				{ System.out.println(ue); }
			}
			else
			{
				m_kernel.StopAllAgents() ;
				m_model.haltAgent();
			}
		}
		catch (Throwable t)
		{
			System.out.println("Caught a throwable event" + t.toString()) ;			
		}
	}
}
