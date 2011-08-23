

import jess.*;
import java.util.*;
import javax.swing.*;

public class JessView implements IView
{
	private static final String INITIAL_FACT_NAME = "initial-fact";
	private static final String SPOT_FACT_NAME = "vacuum.types.spot";
	private static final String POS_FACT_NAME = "vacuum.types.position";
	private static final String ACTION_FACT_NAME = "vacuum.types.action";
	private static final String RADAR_FACT_NAME = "vacuum.types.radar";
	
	private final Model m_model;
	private Rete m_rete = null;
	private Fact m_spotFact = null;
	private Fact m_posFact = null;
	private Fact m_upRadarFact = null;	
	private Fact m_downRadarFact = null;
	private Fact m_leftRadarFact = null;
	private Fact m_rightRadarFact = null;	
	private boolean m_bAllowState = true;
	private boolean m_bRadarSensor = true;
	
	public JessView(Model m)
	{
		m_model = m;
		m_rete = new Rete();		
	}	

	public void init()
	{
		m_model.resetStepCount();
		m_model.resetScore();
		m_model.haltAgent();
	}

	public void setRadarSensor(boolean bEnable)
	{ m_bRadarSensor = bEnable; }

	public void setAllowState(boolean bAllow)
	{ m_bAllowState = bAllow; }
	
	public void run()
	{
		m_model.startAgent();
		try
		{
			// every time we run we start with a blank slate...
			m_rete.executeCommand("(clear)");
			m_rete.executeCommand("(reset)");
			m_spotFact = null;
			
			// the loaded model must define the appropriate templates...
			m_rete.executeCommand("(batch " + m_model.getAgentFile() +  ")");

			while((m_model.getStepCount() > 0) && !m_model.isAgentStopped()) 
			{
				preStep();
				modifyFacts();
				
				m_rete.executeCommand("(facts *)");
				
				m_rete.run(1);
				handleAction();
				postStep();

				try 
				{ Thread.sleep(m_model.getDelay()); }
				catch (InterruptedException ue)
				{ System.out.println(ue); }
			}

			if (!m_model.isAgentStopped())
			{ 
				JOptionPane.showMessageDialog(Main.MAIN_WIN, 
					"Final Score: " + m_model.getScore(),
					"Score", 
					JOptionPane.INFORMATION_MESSAGE);

				init(); 
			}
		}
		catch (JessException e)
		{
			JOptionPane.showMessageDialog(Main.MAIN_WIN, 
					e.toString(),
					"Jess Error", 
					JOptionPane.ERROR_MESSAGE);
			System.out.println(e);
		}
		finally
		{
			m_model.haltAgent();
		}
	}

	protected void preStep()
	{
		m_model.tallyScore();
		System.out.println("Step #" + m_model.getStepCount());
		m_model.decStepCount();
	}

	protected void handleAction() throws JessException
	{
		for (Iterator i = m_rete.listFacts(); i.hasNext(); )
		{
			Fact cf = (Fact)i.next();
			if (cf.getName().endsWith(ACTION_FACT_NAME))
			{
				String action = cf.getSlotValue("move").toString();
				if (action.equals("\"suck\""))
				{ m_model.suck(); }
				else if (action.equals("\"left\""))
				{ m_model.left(); }
				else if (action.equals("\"right\""))
				{ m_model.right(); }
				else if (action.equals("\"up\""))
				{ m_model.up();	}
				else if (action.equals("\"down\""))
				{ m_model.down(); }
				
				m_rete.retract(cf);
				break;
			}
		}
	}

	protected void postStep() throws JessException
	{
		if (!m_model.getAllowState())
		{
			for (Iterator i = m_rete.listFacts(); i.hasNext(); )
			{
				// remove facts that were added by the model because
				// they are not allowed to keep state
				Fact cf = (Fact)i.next();
				if (!cf.getName().endsWith(SPOT_FACT_NAME) &&
				    !cf.getName().endsWith(POS_FACT_NAME) &&	
					!cf.getName().endsWith(RADAR_FACT_NAME) && 
					!cf.getName().endsWith(INITIAL_FACT_NAME) )
				{
					m_rete.retract(cf);
				}
			}
		}
	}
	
	/**
	 * Initialize facts
	 * @author mcohen
	 * @throws JessException
	 */
	protected void initFacts() throws JessException
	{
		m_spotFact = new Fact(SPOT_FACT_NAME, m_rete);
		if (m_model.isDirty())
			m_spotFact.setSlotValue("status", new Value("dirty", RU.STRING));
		else
			m_spotFact.setSlotValue("status", new Value("clean", RU.STRING));
		m_rete.assertFact(m_spotFact);
		

		m_posFact = new Fact(POS_FACT_NAME, m_rete);
		m_posFact.setSlotValue("x", new Value(m_model.agentX(), RU.INTEGER));
		m_posFact.setSlotValue("y", new Value(m_model.agentY(), RU.INTEGER));
		m_rete.assertFact(m_posFact);
		
		if (m_model.getRadarSensor())
		    initRadarFacts(); 
	}
	
	/**
	 * Initialize the radar facts
	 * @author mcohen
	 * @author ogeorgeon detect inner walls
	 * @throws JessException
	 */
	protected void initRadarFacts() throws JessException
	{
		m_rightRadarFact = new Fact(RADAR_FACT_NAME, m_rete);
		m_leftRadarFact = new Fact(RADAR_FACT_NAME, m_rete);
		m_upRadarFact = new Fact(RADAR_FACT_NAME, m_rete);
		m_downRadarFact = new Fact(RADAR_FACT_NAME, m_rete);		
		
		// Radar right
	    int x = (int) (m_model.agentX() + 1);
	    m_rightRadarFact.setSlotValue("dir", new Value("right", RU.STRING));		
	    if (x < m_model.getWidth())
	    {
		    if (m_model.isDirty(x, m_model.agentY()))
		    	m_rightRadarFact.setSlotValue("reading", new Value("dirty", RU.STRING));
		    else
		    	if (m_model.isWall(x, m_model.agentY()))
			    	m_rightRadarFact.setSlotValue("reading", new Value("wall", RU.STRING));
		    	else 
			    	m_rightRadarFact.setSlotValue("reading", new Value("clean", RU.STRING));
	    }
	    else 
	    { 
		    m_rightRadarFact.setSlotValue("reading", new Value("wall", RU.STRING));		
 	    }

	    // Radar left
	    x = (int) (m_model.agentX() - 1);
	    m_leftRadarFact.setSlotValue("dir", new Value("left", RU.STRING));
	    if (x >= 0)
	    {
		    if (m_model.isDirty(x, m_model.agentY()))
		    	m_leftRadarFact.setSlotValue("reading", new Value("dirty", RU.STRING));
		    else
		    	if (m_model.isWall(x, m_model.agentY()))
			    	m_leftRadarFact.setSlotValue("reading", new Value("wall", RU.STRING));
		    	else 
			    	m_leftRadarFact.setSlotValue("reading", new Value("clean", RU.STRING));
	    }
	    else 
	    { 
	    	m_leftRadarFact.setSlotValue("reading", new Value("wall", RU.STRING)); 
	    }

	    // Radar up
	    int y = (int) (m_model.agentY() - 1);
	    m_upRadarFact.setSlotValue("dir", new Value("up", RU.STRING));
	    if (y >= 0)
	    {
		    if (m_model.isDirty(m_model.agentX(), y))
		    	m_upRadarFact.setSlotValue("reading", new Value("dirty", RU.STRING));
		    else
		    	if (m_model.isWall(m_model.agentX(), y))
			    	m_upRadarFact.setSlotValue("reading", new Value("wall", RU.STRING));
		    	else 
			    	m_upRadarFact.setSlotValue("reading", new Value("clean", RU.STRING));
	    }
	    else 
	    { 
	    	m_upRadarFact.setSlotValue("reading", new Value("wall", RU.STRING)); 
	    }

	    //down
	    y = (int) (m_model.agentY() + 1);
	    m_downRadarFact.setSlotValue("dir", new Value("down", RU.STRING));
	    if (y < m_model.getHeight())
	    {
		    if (m_model.isDirty(m_model.agentX(), y))
		    	m_downRadarFact.setSlotValue("reading", new Value("dirty", RU.STRING));
		    else
		    	if (m_model.isWall(m_model.agentX(), y))
			    	m_downRadarFact.setSlotValue("reading", new Value("wall", RU.STRING));
		    	else 
			    	m_downRadarFact.setSlotValue("reading", new Value("clean", RU.STRING));
	    }
	    else 
	    { 
	    	m_downRadarFact.setSlotValue("reading", new Value("wall", RU.STRING)); 
	    }
	    
	    m_rete.assertFact(m_rightRadarFact);
	    m_rete.assertFact(m_leftRadarFact);
	    m_rete.assertFact(m_upRadarFact);
	    m_rete.assertFact(m_downRadarFact);
	}

	protected void modifyFacts() throws JessException
	{
		if (m_spotFact == null)
		{
			initFacts();
		}
		else
		{
			String status = m_spotFact.getSlotValue("status").
				stringValue(m_rete.getGlobalContext());
			if (m_model.isDirty() ) //&& !status.equals("dirty"))
			{
				modifyFact(m_spotFact, "status", new Value("dirty", RU.STRING));
			}
			else //if (!status.equals("clean"))
			{
				modifyFact(m_spotFact, "status", new Value("clean", RU.STRING));
			}
	
			int x = m_posFact.getSlotValue("x").
				intValue(m_rete.getGlobalContext());
			int y = m_posFact.getSlotValue("y").
				intValue(m_rete.getGlobalContext());
			
			//if (m_model.agentX() != x)
			{
				modifyFact(m_posFact, "x", new Value(m_model.agentX(), RU.INTEGER));
			}
			//else if (m_model.agentY() != y)
			{
				modifyFact(m_posFact, "y", new Value(m_model.agentY(), RU.INTEGER));
			}
			
			if (m_model.getRadarSensor())
			    modifyRadarFacts(); 
		}
	}
	
	/**
	 * Modify the radar facts
	 * @author mcohen
	 * @author ogeorgeon detects the inner walls
	 * @throws JessException
	 */
	protected void modifyRadarFacts() throws JessException
	{
		int x = (int) (m_model.agentX() + 1);int y = (int) m_model.agentY();
		boolean notWall = ((x < m_model.getWidth()) && !m_model.isWall(x, y));
		changeRadar(x, y, notWall, m_rightRadarFact); 

	    x = (int) (m_model.agentX() - 1); y = (int) m_model.agentY();
		notWall = ((x >= 0) && !m_model.isWall(x, y));
		changeRadar(x, y, notWall, m_leftRadarFact); 

		x = (int) m_model.agentX(); y = (int) (m_model.agentY() - 1);
		notWall = ((y >= 0) && !m_model.isWall(x, y));
		changeRadar(x, y, notWall, m_upRadarFact);

		x = (int) m_model.agentX(); y = (int) (m_model.agentY() + 1);
		notWall = ((y < m_model.getHeight()) && !m_model.isWall(x, y));
	    changeRadar(x, y, notWall, m_downRadarFact);
	}

	protected void changeRadar(int x, int y, boolean notwall, Fact fact) 
		throws JessException
	{
	    if (notwall)
	    {
		    boolean bDirty = m_model.isDirty(x, y);
			
		    String status = fact.getSlotValue("reading").
				stringValue(m_rete.getGlobalContext());
			if (bDirty ) //&& !status.equals("dirty"))
			{
				modifyFact(fact, "reading", new Value("dirty", RU.STRING));
			}
			else // if (!bDirty && !status.equals("clean"))
			{
				modifyFact(fact, "reading", new Value("clean", RU.STRING));
			}
	    }
	    else 
	    { 
		    String status = fact.getSlotValue("reading").
			stringValue(m_rete.getGlobalContext());
			//if (!status.equals("wall"))
			{
				modifyFact(fact, "reading", new Value("wall", RU.STRING));				
			}
 	    }
	}
	
	protected void modifyFact(Fact f, String name, Value v) throws JessException
	{
		m_rete.retract(f);				
		f.setSlotValue(name, v);
		m_rete.assertFact(f);
	}
	
}
