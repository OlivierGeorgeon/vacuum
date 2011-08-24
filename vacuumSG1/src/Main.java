

import java.text.SimpleDateFormat;
import java.util.*;
import java.awt.*;
import java.awt.event.*; 

import javax.swing.*;

import ernest.EColor;
import ernest.Ernest;

import java.io.*;


public class Main extends JFrame implements Observer, ActionListener, KeyListener
{
	public static final long serialVersionUID = 1;
	
	public static final int RIVER_TOP = 5;
	public static final int RIVER_BOTTOM = 6;
	private int m_previousCounter = 0;
	
	public static JFrame MAIN_WIN;

	private static final String TITLE = "Vacuum (v 3.1)";

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

	private final JFileChooser m_chooser 			= new JFileChooser();
	private final JFileChooser m_boardChooser 		= new JFileChooser(); // Uses specific chooser so that it remembers the path
	private final JFileChooser m_pictureChooser 	= new JFileChooser();
	
	private final MyFileFilter m_fileFilter 		= new MyFileFilter();

    private final Ernest100Model m_model 			    = new Ernest100Model();
 	private final StatusModel m_statusModel         = new StatusModel();
	
	private HelpFrames m_Helpframe;
	private IView m_soar;	 
	private IView m_ernest;
	private JessView m_jess;
	private HumanView m_human;
	private EnvSquare[][] m_grid = null;

	private String logFile;
	public  String BenchmarkFile;

	private JPanel m_board;
	
	///////////
	private EnvironnementFrame m_env;
	
	private final JLabel m_statusBar = new JLabel();
	private JButton m_run = new JButton("Run");
	private JButton m_stop = new JButton("Stop");
	private JButton m_rerun = new JButton("Try again");
	private JButton m_reset = new JButton("Reset");
	private final javax.swing.Timer m_statusTimer =
		new javax.swing.Timer(100, new StatusTimerListener());

	private final ConfigureBoardDlg m_configBoardDlg = 
		new ConfigureBoardDlg(this, m_model);

	private final ConfigureRunDlg m_configRunDlg = 
		new ConfigureRunDlg(this, m_model);

	private String boardTempFile = "board.tmp";

	public static void main(String[] args)
	{
		// Takes no command line argument, instead all is saved in preferences
		
		//if (args.length == 1)
		//	new Main(args[0]);
		//else
		new Main();
	}
	/**
	 * Main
	 * @author mcohen
	 * @author ogeorgeon (Retrieve saved user preferences)
	 * @author mfriedrich (human command and log file)
	 * @param strBoardFile
	 */
	public Main()
	{
		super(TITLE);
		MAIN_WIN = this;
		// Retrieve preferences
		m_model.initPreferences();
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		addWindowListener(new CloseHandler());


		/////////////////
		m_env=new EnvironnementFrame(m_model);
		m_model.setEnvironnement(m_env);
		configureMenu();
		
		
		// Initialize the board
		if (m_model.getRandomBoard())
			m_model.init();
		else
		{
			try
			{ 
				m_model.init(m_model.getBoardFileName()); 
			}
			catch (Exception e)
			{
				JOptionPane.showMessageDialog(this, 
					"Error intializing the board!\n" + 
					e.getClass().toString() + ": " + e.getMessage(),
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
			}
		}

		m_model.setBoardTempFile(boardTempFile);
		m_model.addObserver(this);

		m_stop.setEnabled(false);
		m_run.setEnabled(false);
		m_rerun.setEnabled(false);
		m_reset.setEnabled(true);
		m_stop.addActionListener(this);
		m_run.addActionListener(this);
		m_rerun.addActionListener(this);
		m_reset.addActionListener(this);

		m_board = new JPanel(new GridLayout(m_model.getHeight(), m_model.getWidth()));

		JPanel pageStart = new JPanel();
		pageStart.setPreferredSize(new Dimension(600, 5));
		pageStart.setBackground(new Color(Ernest.COLOR_WALL.getRGB()));
		
		JPanel lineStart = new JPanel();
		lineStart.setPreferredSize(new Dimension(5, 400));
		lineStart.setBackground(new Color(Ernest.COLOR_WALL.getRGB()));

		getContentPane().setLayout(new BorderLayout());
		//getContentPane().add(pageStart, BorderLayout.PAGE_START);
		//getContentPane().add(lineStart, BorderLayout.LINE_START);
		getContentPane().add(new Environnement(m_model),BorderLayout.NORTH );
		getContentPane().add(m_board, BorderLayout.CENTER);
		
		
		
		
		
		
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


		m_statusModel.pushPermStatus("Ready.");
		m_statusBar.setText("Ready.");
		m_statusBar.setPreferredSize(new Dimension(200, m_statusBar.getHeight()));
		m_statusTimer.start();

		m_chooser.setFileFilter(m_fileFilter);
		m_boardChooser.setFileFilter(m_fileFilter);
		//m_pictureChooser.setFileFilter(m_fileFilter);

		// creating a standard Log file name (Maik)
		logFile = new String("VC"
				+ new SimpleDateFormat("yyyyMMMWWHHmmss").format(new Date(
						System.currentTimeMillis())) + ".txt");

		update(null, null);
		pack();
		setVisible(true);
		addKeyListener(this);
		setFocusable(true);
	}
	/**
	 * Performs actions
	 * @author mcohen
	 * @author mfriedrich add a rerun button
	 * @author ogeorgeon  configure log file for soar agent
	 *                    Set the agent's picture
	 */
	public void actionPerformed(ActionEvent e)
	{
		m_model.setEventThread(Thread.currentThread());

		// Run the agent ******
		
		if (e.getSource() == m_run)
		{
			m_model.setBoardTempFile(this.boardTempFile);
			m_model.saveCurrentBoard();
			m_model.startAgent();
			Thread agentThread = null;
			if (m_model.getType() == Model.JESSFILE)
				agentThread = new Thread(getJessView());
			else if (m_model.getType() == Model.SOARFILE)
				agentThread = new Thread(getSoarView());
			else if (m_model.getType() == Model.ERNEST){
				System.out.println("Run Ernest ") ;
				agentThread = new Thread(getErnestView());
			}
			else if (m_model.getType() == Model.HUMANLOGFILE) {
				agentThread = new Thread(getHumanView());
				m_model.setRunHuman(true);
			}
			agentThread.start();
		}
		
		// Rerun from a temp board file
		
		else if (e.getSource() == m_rerun) 
		{
			if (m_model.boardTempExists()) {
				try {
					m_model.init(m_model.getBoardTempFile());
					m_model.startAgent();
					Thread agentThread = null;
					if (m_model.getType() == Model.JESSFILE)
					{
						agentThread = new Thread(getJessView());
						getJessView().init();
					}
					else if (m_model.getType() == Model.SOARFILE)
					{
						agentThread = new Thread(getSoarView());
						getSoarView().init();
					}
					else if (m_model.getType() == Model.ERNEST)
					{
						agentThread = new Thread(getErnestView());
						getErnestView().init();
					}
					else if (m_model.getType() == Model.HUMANLOGFILE) 
					{
						agentThread = new Thread(getHumanView());
						m_model.setRunHuman(true);
						getHumanView().init(); // Olivier: does not work
					}
					agentThread.start();
				} 
				catch (Exception ex) 
				{
					JOptionPane.showMessageDialog(this,
							"Invalid board file!\n" + ex.getClass().toString()
									+ ": " + ex.getMessage(), "Error!",
							JOptionPane.ERROR_MESSAGE);
				} 
				// finally 
				// {
				//	getJessView().init();
				//	getSoarView().init();
				//	getHumanView().init();
				//}
			}
		} 
		
		// Stops the agent *****
		
		else if (e.getSource() == m_stop)
		{
			m_model.save();
			m_model.haltAgent();
			
		}
		
		// Reset the board ******

		else if (e.getSource() == m_reset)
		{
			m_model.setCounter(0);
			if (m_model.getRandomBoard())
			{
			m_model.setRandomBoardParameters(m_model.getWidth(), 
						 m_model.getHeight(),
						 m_model.getDirtyCount());
			m_model.init();
			}
			else
			{
				try
				{ 
					m_model.init(m_model.getBoardFileName()); 
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
			// m_model.setBoardTempFile(this.boardTempFile);
		}
		else if (e.getSource() == m_exit)
		{
			System.exit(0);
		}
	    else if (e.getSource() == m_openHumanAgent) 
	    {
	    	int type = Model.HUMANLOGFILE;

	    	m_model.setLogFile(logFile, type);
	    	m_model.setLogShortFile(new File(logFile).getName());

	    	m_model.setInHumanView(true);
	    	m_model.setAgentShortFile("Human_Interface");
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
	    // Sets the agent's picture
	    else if (e.getSource() == m_setPicture) 
	    {
			// m_fileFilter.set(".GIF", "Pictures gif");
			m_pictureChooser.setVisible(true);
			int returnVal = m_pictureChooser.showOpenDialog(this);
			if(returnVal == JFileChooser.APPROVE_OPTION) 
			{
				try
				{
					File boardFile = m_pictureChooser.getSelectedFile();
					m_model.setPictureFileName(boardFile.getAbsolutePath());

				}
				catch (Exception ex)
				{
				JOptionPane.showMessageDialog(this, 
					"Invalid picture file!\n" + 
					ex.getClass().toString() + ": " + ex.getMessage(),
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
				}
			}	
	    }
	    else if (e.getSource() == m_openJessAgent || 
				 e.getSource() == m_openSoarAgent)
		{
			int type = Model.SOARFILE; 
	    	
			// Sets the log file if any Olivier
			//m_model.setLogFile(logFile);
	    	//m_model.setLogShortFile(new File(logFile).getName());
	    	
			if (e.getSource() == m_openJessAgent)
			{
				m_fileFilter.set(".JESS", "Jess Files");
				type = Model.JESSFILE;
			}
			else
				m_fileFilter.set(".SOAR", "Soar Files");
			
			m_chooser.setVisible(true);
			int returnVal = m_chooser.showOpenDialog(this);
			if(returnVal == JFileChooser.APPROVE_OPTION) 
			{
				try
				{
					String agentFile = 
						m_chooser.getSelectedFile().getCanonicalPath();
					agentFile = "\"" + agentFile.replace('\\', '/') + "\"";
					m_model.setAgentFile(agentFile, type);
					m_model.setAgentShortFile
						(m_chooser.getSelectedFile().getName());
					m_run.setEnabled(true);
				}
				catch (IOException fe)
				{
					System.out.println(fe);
				}
			}			
		}
		// Ernest agent
	    else if (e.getSource() == m_openErnestAgent)
		{
			m_model.setAgentFile("", Model.ERNEST);
			m_model.setAgentShortFile("Ernest");
			m_run.setEnabled(true);
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
					m_model.setBoardFileName(boardFile.getAbsolutePath());
					m_model.init(boardFile.getAbsolutePath()); 
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
			m_configBoardDlg.setVisible(true);
			if (m_configBoardDlg.isOk())
			{
				getJessView().init();
				getSoarView().init();
			}
		}
		else if (e.getSource() == m_configureRun)
		{
			m_configRunDlg.setVisible(true);
			if (m_configRunDlg.isOk())
			{
				getJessView().init();
				getSoarView().init();
			}
		}
		else if ( (e.getSource() == m_simpleAgent) || 
  				  (e.getSource() == m_modelBasedAgent) )
		{
			m_model.setAllowState(m_modelBasedAgent.isSelected());
			getJessView().init();
			getSoarView().setAllowState(m_modelBasedAgent.isSelected());
			getSoarView().init();
		}
		else if (e.getSource() == m_radarSensor)
		{
			m_model.setRadarSensor(m_radarSensor.isSelected());
			getJessView().init();
			getSoarView().setRadarSensor(m_radarSensor.isSelected());
			getSoarView().init();
		}
		else if (e.getSource() == m_movePunish)
		{
			m_model.setPenalizeForMovement(m_movePunish.isSelected());
			getJessView().init();
			getSoarView().init();
		}
		else if (e.getSource() == m_randomBoard)
		{
			m_model.setRandomBoard(m_randomBoard.isSelected());
			if (m_model.getRandomBoard())
				m_model.init();
			else
			{
				try
				{
					m_model.init(m_model.getBoardFileName());
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
			getJessView().init();
			getSoarView().init();
		}
		else if (e.getSource() == m_speakAloud)
		{
			m_model.setSpeakAloud(m_speakAloud.isSelected());
			getJessView().init();
			getSoarView().init();
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
		m_model.putPreferences();
	}

	public void update(Observable o, Object arg)
	{
		if (m_model.isAgentStopped())
		{
			if (m_model.getScore() != 0)
				m_model.setLastFinalScore(m_model.getScore());
 
			m_file.setEnabled(true);
			m_options.setEnabled(true);
			m_help.setEnabled(true);
			if (m_model.getLastFinalScore() != 0) {
				m_statusModel.pushPermStatus("Ready. Final Score last run: "
						+ m_model.getLastFinalScore());
			} else {
				m_statusModel.pushPermStatus("Ready.");
			}

			if ((m_model.getAgentFile() != null)
				    || (m_model.getInHumanView() == true)) 
			{

				if (m_model.boardTempExists()) {
					m_rerun.setEnabled(true);
				}
				m_model.setRunHuman(false);
				m_run.setEnabled(true);
				m_reset.setEnabled(true);
				m_stop.setEnabled(false);
				setTitle(TITLE + " - " + m_model.getAgentShortFile());
			}
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

			m_statusModel.pushPermStatus("Step #" + (m_model.getStepCount() +1 ) + 
							   ":  SCORE: " + m_model.getScore());
		}

		drawGrid();
		getContentPane().validate();
		repaint();
		m_env.repaint();
	}
	/**
	 * Update all the squares in the grid from the model
	 */
	private void drawGrid()
	{
		resizeGrid();
		
		// Fish move to the right when they are in the river flow
		
		if (m_model.getCounter() == 0) m_previousCounter = 0;
		
		if (m_model.getCounter() > m_previousCounter)
		//if ((m_model.getCounter() % 2 == 0) && (m_model.getCounter() > m_previousCounter))
		{
			for (int i = m_model.getWidth() - 2; i >= 0 ; i--)
				for (int j = RIVER_TOP; j <= RIVER_BOTTOM; j++)
					if (m_model.isDirty(i, j))
					{
						if (i < m_model.getWidth() - 2)
							m_model.setDirty(i + 1, j, m_model.getDirty(i, j));
						m_model.setDirty(i, j, Model.EMPTY);
					}
			m_previousCounter = m_model.getCounter();
		}
		
		for (int y = 0; y < m_model.getHeight(); y++)
		{
			for (int x = 0; x < m_model.getWidth(); x++)
			{
				
				// Update the square visualization.
				
				m_grid[y][x].udateBackground();
			}
		}
		
		// handle mouse events from continuous environment
		int c= m_env.m_env.getClicked();
		if (c == 1)
		{
			if (m_model.isWall(m_env.m_env.m_clickX,m_env.m_env.m_clickY))
			{
				m_model.setWall(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.EMPTY);
				m_model.traceUserEvent("remove_wall", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
			else
			{
				m_model.setWall(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.WALL);
				m_model.traceUserEvent("add_wall", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
		}
		if (c == 2)
		{
			if (m_model.isDirty(m_env.m_env.m_clickX,m_env.m_env.m_clickY))
			{
				m_model.setDirty(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.EMPTY);
				m_model.traceUserEvent("remove_water", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
			else
			{
				m_model.setDirty(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.DIRTY);
				m_model.traceUserEvent("add_water", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
		}
		if (c == 3)
		{
			if (m_model.isDirty(m_env.m_env.m_clickX,m_env.m_env.m_clickY))
			{
				m_model.setDirty(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.EMPTY);
				m_model.traceUserEvent("remove_food", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
			else
			{
				m_model.setDirty(m_env.m_env.m_clickX, m_env.m_env.m_clickY, Model.FOOD);
				m_model.traceUserEvent("add_food", m_env.m_env.m_clickX, m_env.m_env.m_clickY);
			}
		}
		if (c == 4)
			m_model.toggleNight();
		
		m_env.m_env.repaint();
		
	}

	/**
	 * Creates the grid board if it does not exist or if the grid's size has changed.
	 * @author mchohen
	 */
	private void resizeGrid()
	{

		
		if (m_grid == null || 
			m_grid.length != m_model.getHeight() ||
			m_grid[0].length != m_model.getWidth())
		{
			m_board.removeAll();
			m_board.setBackground(new Color(Ernest.COLOR_WALL.getRGB()));
			m_board.setLayout(new GridLayout(m_model.getHeight(),  m_model.getWidth()));
			//m_board.setPreferredSize(new Dimension(600,450));
			m_grid = new EnvSquare[m_model.getHeight()][m_model.getWidth()];
			
			for (int y = 0; y < m_model.getHeight(); y++)
			{
				for (int x = 0; x < m_model.getWidth(); x++)
				{
                	m_grid[y][x] = new EnvSquare(x, y, m_model);
                	m_grid[y][x].setPreferredSize(new Dimension(37,37));
                	m_board.add(m_grid[y][x]);
				}
			}
			//m_env=new Environnement(m_model);
			//m_env.setPreferredSize(new Dimension(400,400));
			//m_board.add(m_env);
		}
	}

	/**
	 * Draw the Menus
	 * @author mchohen
	 * @author mfriedrich
	 * @author ogeorgeon 
	 */
	private void configureMenu()
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
		m_modelBasedAgent.setSelected(m_model.getAllowState());
		m_simpleAgent.setSelected(!m_model.getAllowState());
		m_loadBoard.addActionListener(this);
		m_saveBoard.addActionListener(this);
		m_configureBoard.addActionListener(this);
		m_configureRun.addActionListener(this);

		m_radarSensor.setSelected(m_model.getRadarSensor());
		m_radarSensor.addActionListener(this);
		m_movePunish.addActionListener(this);
		m_randomBoard.setSelected(m_model.getRandomBoard());
		m_randomBoard.addActionListener(this);
		m_speakAloud.setSelected(m_model.getSpeakAloud());
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
		setJMenuBar(bar);
		//m_env.setJMenuBar(bar);
	}

	private class StatusTimerListener implements ActionListener
	{
		public void actionPerformed(ActionEvent e)
		{
			m_statusBar.setText(m_statusModel.getNextStatus());
		}
	}

	private class CloseHandler extends WindowAdapter
	{
		public void windowClosing(WindowEvent e)
		{ m_statusTimer.stop(); }
	}
	
	/**
	 * Loads the Jess execution class  
	 * @author mcohen
	 */
	private IView getJessView()
	{
		try
		{
			if (m_jess == null)
				m_jess = new JessView(m_model);
		}
		catch (NoClassDefFoundError e)
		{
			JOptionPane.showMessageDialog(this, 
					"Error loading the Jess engine!\n" + 
					"Please restart the environment with jess.jar included in the classpath.",
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
		}
		return m_jess;
	}
	
	/**
	 * Loads the Soar execution class  
	 * @author mcohen
	 * @author ogeorgeon adapt to Soar 9 
	 */
	private IView getSoarView()
	{
		try
		{
			if (m_soar == null)
				m_soar = new Soar9View(m_model);		
		}
		catch (NoClassDefFoundError e)
		{
			JOptionPane.showMessageDialog(this, 
					"Error loading the Soar engine!\n" + 
					"Please restart the environment with sml.jar included in the classpath.", 
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
		}
		return m_soar;
	}

	/**
	 * Loads the Ernest execution class  
	 * @author ogeorgeon 
	 */
	private IView getErnestView()
	{
		try
		{
			if (m_ernest == null)
				m_ernest = new ErnestView(m_model);		
		}
		catch (NoClassDefFoundError e)
		{
			JOptionPane.showMessageDialog(this, 
					"Error loading the Ernest engine!\n" + 
					"Please restart the environment with ernest.jar included in the classpath.", 
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
		}
		return m_ernest;
	}

	/**
	 * Handle human commands  
	 * @author mfriedrich
	 */
	private HumanView getHumanView() 
	{
		try {
			if (m_human == null)
				m_human = new HumanView(m_model);
		} catch (NoClassDefFoundError e) {
			JOptionPane.showMessageDialog(this,
					"Error loading the Human engine!\n", "Error!",
					JOptionPane.ERROR_MESSAGE);
		}
		return m_human;
	}

	public void keyTyped(KeyEvent e) {
	}

	public void keyPressed(KeyEvent e) {

	}

	public void keyReleased(KeyEvent e) 
	{
		if ((m_model.getInHumanView()) && (m_model.getRunHuman())) {
			m_human.getQueryHuman().offer(e);
		}
	}
}
