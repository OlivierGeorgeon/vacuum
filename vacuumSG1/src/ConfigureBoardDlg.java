

import java.awt.*;
import javax.swing.*;

public class ConfigureBoardDlg extends Dialog
{
	public static final long serialVersionUID = 1; 
	
    public ConfigureBoardDlg(JFrame parent, Model model)
	{
        super(parent, "Configure Board");

		m_model = model;

        JPanel center = new JPanel(new GridLayout(3, 2));
        center.add(new Label("Width: "));
        center.add(m_width);
        center.add(new Label("Height: "));
        center.add(m_height);
		center.add(new Label("# of Dirty Squares: "));
		center.add(m_dirty);
        getContentPane().add(center, BorderLayout.CENTER);
		
		setSize(250, 150);
	}

	public void show()
	{
		m_width.setText(""+m_model.getWidth());
		m_height.setText(""+m_model.getHeight());
		m_dirty.setText(""+m_model.getDirtyCount());
		super.show();
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
			int iWidth = Integer.parseInt(m_width.getText());
			int iHeight = Integer.parseInt(m_height.getText());
			int iDirty = Integer.parseInt(m_dirty.getText());

			if ( (iWidth <=0) || 
				 (iHeight <= 0) || 
				 (iDirty < 0) || 
				 (iDirty > (iWidth * iHeight)) )
			{
				JOptionPane.showMessageDialog(this, 
					"Please enter a valid integer!",
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				bRet = false;
			}

			if (bRet)
			{
				m_model.setRandomBoardParameters(iWidth,iHeight,iDirty);
                // redraw the grid before having index out of range
				m_model.init();
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
		m_width.setText("" + m_model.INIT_W);
		m_height.setText("" + m_model.INIT_H);
		m_dirty.setText("" + m_model.INIT_DIRTY);    	
    }

    private final JTextField m_width = new JTextField(50);
    private final JTextField m_height = new JTextField(50);
	private final JTextField m_dirty = new JTextField(50);

	private final Model m_model;
}
