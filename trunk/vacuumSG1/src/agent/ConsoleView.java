package agent;


import java.util.*;

public class ConsoleView implements Observer
{
	private final Environment m_env;

	public static void main(String[] args)
	{
/*		Model m = new Model();
		m.init();
		new ConsoleView(m);
		m.left();
		m.suck();*/
	}

	public ConsoleView(Environment env)
	{
		m_env = env;
		m_env.addObserver(this);
		update(null, null);
	}

	public void update(Observable o, Object arg)
	{
		StringBuffer sb = new StringBuffer();
		sb.append("Robot at: "); 
		sb.append(m_env.agentX(0));
			sb.append(", ");
			sb.append(m_env.agentY(0));
		for (int y = 0; y < m_env.getHeight(); y++)
		{
			sb.append("\n");
			for (int x = 0; x < m_env.getWidth(); x++)
			{
				if (m_env.isFood(x,y) || m_env.isAlga(x,y))
					sb.append("DIRTY ");
				else
					sb.append("EMPTY ");
			}
		}
		System.out.println(sb.toString());
	}
}