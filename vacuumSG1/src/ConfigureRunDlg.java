

import java.awt.*;
import javax.swing.*;

public class ConfigureRunDlg extends Dialog
{
	public static final long serialVersionUID = 1;
	
    public ConfigureRunDlg(JFrame parent, Model model)
	{
        super(parent, "Configure Run");
		
		m_model = model;

        JPanel center = new JPanel(new GridLayout(2, 2));
        center.add(new Label("Time Steps: "));
        center.add(m_steps);
        center.add(new Label("Delay Between Steps (ms): "));
        center.add(m_delay);
        getContentPane().add(center, BorderLayout.CENTER);

		setSize(375, 130);
	}

	public void setVisible(boolean b)
	{
		m_steps.setText(""+m_model.getTotalSteps());
		m_delay.setText(""+m_model.getDelay());
		super.setVisible(b);
	}

	/**
	 * Handles the modifications
	 * @autor mcohen
	 * @author ogeorgeon save the modifications as preferences
	 */
    protected boolean handleOk()
	{
		boolean bRet = true;
		try 
		{
			int iSteps = Integer.parseInt(m_steps.getText());
			int iDelay = Integer.parseInt(m_delay.getText());

			if (iSteps <=0)
			{
				JOptionPane.showMessageDialog(this, 
					"Please enter a positive integer for steps!",
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				bRet = false;
			}
			else if (iDelay <= 0)
			{
				JOptionPane.showMessageDialog(this, 
					"Delay must be a positive integer!",
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				bRet = false;
			}

			if (bRet)
			{
				m_model.setTotalSteps(iSteps);
				m_model.setDelay(iDelay);
				m_model.putPreferences();
			}
		}
		catch (NumberFormatException e)
		{
			JOptionPane.showMessageDialog(this, 
				"Please enter a valid integer!",
				"Error!", 
				JOptionPane.ERROR_MESSAGE);
			bRet = false;
		}
		return bRet;
	}

	/**
	 * Handles default button
	 * @author ogeorgeon Reset the default values
	 */
    protected void handleDefault()
    {
    	m_steps.setText("" + m_model.INIT_STEPS);
    	m_delay.setText("" + m_model.INIT_DELAY);
    }

    private final JTextField m_steps = new JTextField(50);
    private final JTextField m_delay = new JTextField(50);

	private final Model m_model;
}
