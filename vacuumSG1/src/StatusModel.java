

import java.util.*;

public class StatusModel
{
	public void pushPermStatus(String status)
	{ 
		synchronized (m_strStatusStack)
		{
			m_strStatusStack.addFirst(status);
		}
	}

	public void popPermStatus()
	{ 	   
		synchronized (m_strStatusStack)
		{
			m_strStatusStack.removeFirst();
		}
	}

	public void addTransStatus(String status)
	{
		synchronized (m_strStatusQueue)
		{
			m_strStatusQueue.add(status);
		}
	}

	public String getNextStatus()
	{
		String strRet;
		synchronized (m_strStatusQueue)
		{
			if (m_strStatusQueue.size() == 0)
			{
				synchronized (m_strStatusStack)
				{
					strRet = (String)m_strStatusStack.getFirst();
				}
			}
			else
			{
				strRet = (String)m_strStatusQueue.removeFirst();
			}
		}
		return strRet;
	}

	private final LinkedList<String> m_strStatusStack 
		= new LinkedList<String>();
	private final LinkedList<String> m_strStatusQueue 
		= new LinkedList<String>();
}