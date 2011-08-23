

import java.util.*;

public class ConsoleView implements Observer
{
	private final Model m_model;

	public static void main(String[] args)
	{
/*		Model m = new Model();
		m.init();
		new ConsoleView(m);
		m.left();
		m.suck();*/
	}

	public ConsoleView(Model m)
	{
		m_model = m;
		m_model.addObserver(this);
		update(null, null);
	}

	public void update(Observable o, Object arg)
	{
		StringBuffer sb = new StringBuffer();
		sb.append("Robot at: "); 
		sb.append(m_model.agentX());
			sb.append(", ");
			sb.append(m_model.agentY());
		for (int y = 0; y < m_model.getHeight(); y++)
		{
			sb.append("\n");
			for (int x = 0; x < m_model.getWidth(); x++)
			{
				if (m_model.isDirty(x,y))
					sb.append("DIRTY ");
				else
					sb.append("EMPTY ");
			}
		}
		System.out.println(sb.toString());
	}
}