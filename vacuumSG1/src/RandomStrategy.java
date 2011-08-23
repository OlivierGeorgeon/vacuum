

import java.util.Random;

import jess.Activation;
import jess.Strategy;

public class RandomStrategy implements Strategy
{

	public RandomStrategy()
	{
		super();
		// TODO Auto-generated constructor stub
	}

	public int compare(Activation arg0, Activation arg1)
	{
		if (arg0.getSalience()  == arg1.getSalience())
		{
			if (m_rand.nextInt(2) == 0)
				return -1;
			else return 1;
		}
		else if (arg0.getSalience() > arg1.getSalience())
		{
			return 1;
		}
		else 
		{
			return -1;
		}
	}

	public String getName()
	{
		return "random strategy that respects salience";
	}
	
	private static Random m_rand = new Random(); 
}
