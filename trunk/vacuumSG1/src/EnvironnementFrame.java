import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Graphics2D;
import java.awt.GridLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.Observable;
import java.util.Observer;

import javax.imageio.ImageIO;
import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFileChooser;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;

import ernest.Ernest;




public class EnvironnementFrame extends JFrame implements Observer, ActionListener, KeyListener{
	
	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	public Environnement m_env;
	
	public int indexImage;
	
	/////////////////////////////////////////////////
	private final JMenu m_file 		= new JMenu("File");
	private final JMenu m_options 	= new JMenu("Options");
	private final JMenu m_help 		= new JMenu("Help");
	
	private final JMenuItem m_KeyboardLayout 	= new JMenuItem("Key Stroke Short Cuts");
	private final JMenuItem m_aboutVacuum 		= new JMenuItem("About Vacuumcleaner");
	
	private final JMenuItem m_openJessAgent 			= new JMenuItem("Open Jess Agent...");
	private final JMenuItem m_openSoarAgent 			= new JMenuItem("Open Soar Agent...");
	private final JMenuItem m_openErnestAgent 			= new JMenuItem("Open Ernest Agent");
	private final JMenuItem m_openHumanAgent 			= new JMenuItem("Start Human Interface");
	private final JMenuItem m_setLogFile 				= new JMenuItem("Set the Log File");
	private final JMenuItem m_setBenchmarkFile 			= new JMenuItem("Set the benchmark File");
	private final JMenuItem m_setPicture 				= new JMenuItem("Set picture");
	private final JMenuItem m_exit 						= new JMenuItem("Exit");
	private final JMenuItem m_configureBoard 			= new JMenuItem("Configure Random Board...");
	private final JMenuItem m_configureRun        		= new JMenuItem("Configure Run...");
	private final JCheckBoxMenuItem m_randomBoard 		= new JCheckBoxMenuItem("Random Board");
	private final JMenuItem m_loadBoard           		= new JMenuItem("Choose Board...");
	private final JMenuItem m_saveBoard               	= new JMenuItem("Save Board...");
	private final JCheckBoxMenuItem m_simpleAgent     	= new JCheckBoxMenuItem("Simple Reflex Agent");
	private final JCheckBoxMenuItem m_modelBasedAgent 	= new JCheckBoxMenuItem("Model Based Agent");
	private final JCheckBoxMenuItem m_radarSensor     	= new JCheckBoxMenuItem("Enable Radar Sensor");
	private final JCheckBoxMenuItem m_movePunish      	= new JCheckBoxMenuItem("Penalize for Movement");
	private final JCheckBoxMenuItem m_speakAloud      	= new JCheckBoxMenuItem("Speak Aloud");
	
	
	private final JLabel m_statusBar = new JLabel();
	private JButton m_run = new JButton("Run");
	private JButton m_stop = new JButton("Stop");
	private JButton m_rerun = new JButton("Try again");
	private JButton m_reset = new JButton("Reset");
	
	private HelpFrames m_Helpframe;
	private final JFileChooser m_chooser 			= new JFileChooser();
	private final JFileChooser m_boardChooser 		= new JFileChooser(); // Uses specific chooser so that it remembers the path
	private final JFileChooser m_pictureChooser 	= new JFileChooser();
	
	private final MyFileFilter m_fileFilter 		= new MyFileFilter();
	
	private String logFile;
	public  String BenchmarkFile;
	private String boardTempFile = "board.tmp";
	//////////////////////////////////////////
	
	
    public EnvironnementFrame(Model m){
	
    	this.setTitle("Ernest");
    	this.setSize(600, 600);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);
    
    	m_env = new Environnement(m);
    	
    	configureMenu(m);
    	
    	this.setContentPane(m_env);
    
    	m_stop.setEnabled(false);
		m_run.setEnabled(false);
		m_rerun.setEnabled(false);
		m_reset.setEnabled(true);
		m_stop.addActionListener(this);
		m_run.addActionListener(this);
		m_rerun.addActionListener(this);
		m_reset.addActionListener(this);
    	


		JPanel pageStart = new JPanel();
		pageStart.setPreferredSize(new Dimension(600, 5));
		pageStart.setBackground(new Color(Ernest.COLOR_WALL.getRGB()));
		
		JPanel lineStart = new JPanel();
		lineStart.setPreferredSize(new Dimension(5, 400));
		lineStart.setBackground(new Color(Ernest.COLOR_WALL.getRGB()));

		getContentPane().setLayout(new BorderLayout());
		
		JPanel buttonPanel = new JPanel();

		buttonPanel.addKeyListener(this);
		
		
		buttonPanel.add(m_rerun);
		buttonPanel.add(m_reset);
		buttonPanel.add(m_run);
		buttonPanel.add(m_stop);
		JPanel statusPanel = new JPanel(new BorderLayout());
		statusPanel.add(m_statusBar, BorderLayout.CENTER);
		statusPanel.add(buttonPanel, BorderLayout.EAST);
		statusPanel.setBorder(BorderFactory.createRaisedBevelBorder());
		getContentPane().add(statusPanel, BorderLayout.SOUTH);
		
		indexImage=0;
    }
    
	/**
	 * Draw the Menus
	 * @author mchohen
	 * @author mfriedrich
	 * @author ogeorgeon 
	 */
	private void configureMenu(Model model)
	{
		// mnemonics...
		m_file.setMnemonic(KeyEvent.VK_F);
		m_openJessAgent.setMnemonic(KeyEvent.VK_J);
		m_openSoarAgent.setMnemonic(KeyEvent.VK_A);
		m_openErnestAgent.setMnemonic(KeyEvent.VK_E);
		m_openHumanAgent.setMnemonic(KeyEvent.VK_H);
		m_exit.setMnemonic(KeyEvent.VK_X);

		m_options.setMnemonic(KeyEvent.VK_O);
		m_randomBoard.setMnemonic(KeyEvent.VK_A);
		m_configureBoard.setMnemonic(KeyEvent.VK_B);
		m_configureRun.setMnemonic(KeyEvent.VK_R);
		m_loadBoard.setMnemonic(KeyEvent.VK_L);
		m_saveBoard.setMnemonic(KeyEvent.VK_V);
		m_simpleAgent.setMnemonic(KeyEvent.VK_S);
		m_modelBasedAgent.setMnemonic(KeyEvent.VK_M);
		m_radarSensor.setMnemonic(KeyEvent.VK_E);
		m_movePunish.setMnemonic(KeyEvent.VK_P);

		// file menu...
		m_file.add(m_openJessAgent);
		m_file.add(m_openSoarAgent);
		m_file.add(m_openErnestAgent);
		m_file.add(m_openHumanAgent);
		m_file.addSeparator();
		m_file.add(m_setLogFile);
		m_file.add(m_setBenchmarkFile);
		m_file.addSeparator();
		m_file.add(m_setPicture);
		m_file.addSeparator();
		m_file.add(m_exit);
		
		m_openJessAgent.addActionListener(this);
		m_openSoarAgent.addActionListener(this);
		m_openErnestAgent.addActionListener(this);
		m_openHumanAgent.addActionListener(this);
		m_setLogFile.addActionListener(this);
		m_setBenchmarkFile.addActionListener(this);
		m_setPicture.addActionListener(this);
		m_exit.addActionListener(this);

		// options menu...
		m_options.add(m_configureRun);
		m_options.add(m_radarSensor);
		m_options.add(m_movePunish);
		m_options.addSeparator();
		m_options.add(m_randomBoard);
		m_options.add(m_configureBoard);
		m_options.add(m_loadBoard);
		m_options.add(m_saveBoard);
		m_options.addSeparator();
		ButtonGroup group = new ButtonGroup();
		m_options.add(m_simpleAgent);
		m_options.add(m_modelBasedAgent);
		group.add(m_simpleAgent);
		group.add(m_modelBasedAgent);
		m_options.addSeparator();
		m_options.add(m_speakAloud);

		m_simpleAgent.addActionListener(this);
		m_modelBasedAgent.addActionListener(this);
		m_loadBoard.addActionListener(this);
		m_saveBoard.addActionListener(this);
		m_configureBoard.addActionListener(this);
		m_configureRun.addActionListener(this);

		m_radarSensor.addActionListener(this);
		m_movePunish.addActionListener(this);
		m_randomBoard.setSelected(model.getRandomBoard());
		m_randomBoard.addActionListener(this);
		m_speakAloud.setSelected(model.getSpeakAloud());
		m_speakAloud.addActionListener(this);

		// help menu...
		m_help.add(m_aboutVacuum);
		m_help.add(m_KeyboardLayout);
		m_KeyboardLayout.addActionListener(this);
		m_aboutVacuum.addActionListener(this);

		// menu bar...
		JMenuBar bar = new JMenuBar();
		bar.add(m_file);
		bar.add(m_options);
		bar.add(m_help);
		this.setJMenuBar(bar);
	}
    
    
    public void paint(){
    	m_env.repaint();
    }


	@Override
	public void keyPressed(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void keyReleased(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void keyTyped(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}


	public void actionPerformed(ActionEvent e)
	{
		//m_env.m_model.setEventThread(Thread.currentThread());

		// Run the agent ******
		
		if (e.getSource() == m_run)
		{
			m_env.m_model.setBoardTempFile(this.boardTempFile);
			m_env.m_model.saveCurrentBoard();
			m_env.m_model.startAgent();
			Thread agentThread = null;
			System.out.println("Run Ernest ");
			agentThread.start();
		}
		
		// Rerun from a temp board file
		
		else if (e.getSource() == m_rerun) 
		{
			if (m_env.m_model.boardTempExists()) {
				try {
					m_env.m_model.init(m_env.m_model.getBoardTempFile());
					m_env.m_model.startAgent();
					Thread agentThread = null;
					agentThread.start();
				} 
				catch (Exception ex) 
				{
					JOptionPane.showMessageDialog(this,
							"Invalid board file!\n" + ex.getClass().toString()
									+ ": " + ex.getMessage(), "Error!",
							JOptionPane.ERROR_MESSAGE);
				} 
			}
		} 
		
		// Stops the agent *****
		
		else if (e.getSource() == m_stop)
		{
			m_env.m_model.haltAgent();
		}
		
		// Reset the board ******

		else if (e.getSource() == m_reset)
		{
			m_env.m_model.setCounter(0);
			if (m_env.m_model.getRandomBoard())
			{
			m_env.m_model.setRandomBoardParameters(m_env.m_model.getWidth(), 
						 m_env.m_model.getHeight(),
						 m_env.m_model.getDirtyCount());
			m_env.m_model.init();
			}
			else
			{
				try
				{ 
					m_env.m_model.init(m_env.m_model.getBoardFileName()); 
				}
				catch (Exception ex)
				{
					JOptionPane.showMessageDialog(this, 
						"Error while initializing the board! (Check picture file and board file)\n" + 
						e.getClass().toString() + ": " + ex.getMessage(),
						"Error!", 
						JOptionPane.ERROR_MESSAGE);
				}
			}
			// m_env.m_model.setBoardTempFile(this.boardTempFile);
		}
		else if (e.getSource() == m_exit)
		{
			System.exit(0);
		}
	    else if (e.getSource() == m_setLogFile) 
	    {
	    	m_fileFilter.set(".txt", "Log File (.txt)");

	    	m_chooser.setVisible(true);

	    	File saverFile = null;
	    	m_chooser.setCurrentDirectory(new File(System
				.getProperty("user.dir")));
	    	m_chooser.setSelectedFile(new File(System.getProperty("user.dir")
				+ "//" + logFile));
	    	int returnVal = m_chooser.showSaveDialog(this);

	    	if (returnVal == JFileChooser.APPROVE_OPTION) 
	    	{
	    		saverFile = m_chooser.getSelectedFile();
	    		try {
	    			logFile = saverFile.getCanonicalPath();
	    			logFile = logFile.replace('\\', '/');
	    			//if (!logFile.endsWith(".txt"))
	    				//logFile = logFile + ".txt";
	    			//if (logFile.endsWith(".xml"))
	    			//{
	    				
	    			//}
	    		}
	    		catch (IOException e1) 
	    		{
	    			e1.printStackTrace();
	    		}
	    	}
		}
	    else if (e.getSource() == m_setBenchmarkFile) 
	    {
	    	m_fileFilter.set(".txt", "Benchmark File (.txt)");

	    	m_chooser.setVisible(true);

	    	File saverFile = null;
	    	m_chooser.setCurrentDirectory(new File(System
				.getProperty("user.dir")));
	    	m_chooser.setSelectedFile(new File(System.getProperty("user.dir")
				+ "//" + BenchmarkFile));
	    	int returnVal = m_chooser.showSaveDialog(this);

	    	if (returnVal == JFileChooser.APPROVE_OPTION) 
	    	{
	    		saverFile = m_chooser.getSelectedFile();
	    		try {
	    			BenchmarkFile = saverFile.getCanonicalPath();
	    			BenchmarkFile = BenchmarkFile.replace('\\', '/');
	    		}
	    		catch (IOException e1) 
	    		{
	    			e1.printStackTrace();
	    		}
	    	}
		}
		// Load Board
		else if (e.getSource() == m_loadBoard)
		{
			m_fileFilter.set(".TXT", "Board Files");
			m_boardChooser.setVisible(true);
			int returnVal = m_boardChooser.showOpenDialog(this);
			if(returnVal == JFileChooser.APPROVE_OPTION) 
			{
				try
				{
					File boardFile = m_boardChooser.getSelectedFile();
					m_env.m_model.setBoardFileName(boardFile.getAbsolutePath());
					m_env.m_model.init(boardFile.getAbsolutePath()); 
				}
				catch (Exception ex)
				{
				JOptionPane.showMessageDialog(this, 
					"Invalid board file!\n" + 
					ex.getClass().toString() + ": " + ex.getMessage(),
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				}
			}			
		}
		else if (e.getSource() == m_configureBoard)
		{
			//m_configBoardDlg.setVisible(true);

		}
		else if (e.getSource() == m_configureRun)
		{
			//m_configRunDlg.setVisible(true);

		}
		else if (e.getSource() == m_movePunish)
		{
			m_env.m_model.setPenalizeForMovement(m_movePunish.isSelected());
		}
		else if (e.getSource() == m_randomBoard)
		{
			m_env.m_model.setRandomBoard(m_randomBoard.isSelected());
			if (m_env.m_model.getRandomBoard())
				m_env.m_model.init();
			else
			{
				try
				{
					m_env.m_model.init(m_env.m_model.getBoardFileName());
				}
				catch (Exception ex)
				{
				JOptionPane.showMessageDialog(this, 
					"Invalid board file!\n" + 
					ex.getClass().toString() + ": " + ex.getMessage(),
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				}
			}

		}
		else if (e.getSource() == m_speakAloud)
		{
			m_env.m_model.setSpeakAloud(m_speakAloud.isSelected());;
		}
	    else if (e.getSource() == m_KeyboardLayout) 
	    {
		    m_Helpframe = new HelpFrames(this, "Keyboard");
	    }
	    else if (e.getSource() == m_aboutVacuum) 
	    {
    		m_Helpframe = new HelpFrames(this, "About");
	    }
		// Save the preferences
		m_env.m_model.putPreferences();
	}


	public void update(Observable o, Object arg)
	{
		if (m_env.m_model.isAgentStopped())
		{
			if (m_env.m_model.getScore() != 0)
				m_env.m_model.setLastFinalScore(m_env.m_model.getScore());
 
			m_file.setEnabled(true);
			m_options.setEnabled(true);
			m_help.setEnabled(true);

			if (m_env.m_model.boardTempExists()) {
				m_rerun.setEnabled(true);
			}
			m_run.setEnabled(true);
			m_reset.setEnabled(true);
			m_stop.setEnabled(false);
			setTitle("Ernest" + " - " + m_env.m_model.getVersion());
		}
		else
		{
			m_file.setEnabled(false);
			m_options.setEnabled(false);
			m_help.setEnabled(false);
			m_run.setEnabled(false);
			m_reset.setEnabled(false);
			m_rerun.setEnabled(false);

			m_stop.setEnabled(true);
			m_stop.setSelected(true);

		}
		getContentPane().validate();
		repaint();
		m_env.repaint();
	}
	
	
	public void saveImage(){
		
		String path="/home/simon/Bureau/Ernest/";
		
		if (indexImage<10) path+="000"+indexImage+".jpg";
		else if (indexImage<100) path+="00"+indexImage+".jpg";
		else                     path+="0" +indexImage+".jpg";
		
		 BufferedImage image = new BufferedImage(this.getWidth(),
                this.getHeight(),
                BufferedImage.TYPE_INT_RGB);
		 		Graphics2D g2 = image.createGraphics();
		 		this.paint(g2);
		 		g2.dispose();
		 		try {
		 			ImageIO.write(image, "JPEG", new File(path));
		 		} catch (Exception e) { }
		 		indexImage++;
	}

}
