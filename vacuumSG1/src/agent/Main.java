package agent;


import java.text.SimpleDateFormat;
import java.util.*;
import java.util.List;
import java.awt.*;
import java.awt.event.*; 

import javax.swing.*;

import ernest.Ernest;

import java.io.*;

public class Main extends JFrame implements Observer, ActionListener, KeyListener
{
	public final long serialVersionUID = 1;
	
	public static JFrame MAIN_WIN;

	private static final String TITLE = "Vacuum-SG";

	private final JMenu m_file 						= new JMenu("File");
	private final JMenu m_options 					= new JMenu("Options");
	private final JMenu m_help 						= new JMenu("Help");
	private final JMenuItem m_KeyboardLayout 		= new JMenuItem("Key Stroke Short Cuts");
	private final JMenuItem m_aboutVacuum 			= new JMenuItem("About Vacuumcleaner");
	private final JMenuItem m_exit 					= new JMenuItem("Exit");
	private final JMenuItem m_configureRun        	= new JMenuItem("Configure Run...");
	private final JMenuItem m_loadBoard           	= new JMenuItem("Choose Board...");
	private final JCheckBoxMenuItem m_speakAloud    = new JCheckBoxMenuItem("Speak Aloud");
	
	private final JFileChooser m_chooser 			= new JFileChooser();
	private final JFileChooser m_boardChooser 		= new JFileChooser(); // Uses specific chooser so that it remembers the path
	
	private final MyFileFilter m_fileFilter 		= new MyFileFilter();

    private static Environment m_environment;
    public static ArrayList<ErnestModel> m_modelList;
    
    private static int version;
	
	private HelpFrames m_Helpframe;
	private ErnestView m_ernest;

	private JPanel m_board;
	
	private boolean stopAll=true;
	
	/////////////////////////////////////////////////////////////////////////////////////////
	
	private EnvironnementPanel m_envPanel;
	
	private boolean m_halt = true;
	protected int m_w;
	protected int m_h;
	
	
	private final JLabel m_statusBar = new JLabel();
	private JButton m_run = new JButton("Run");
	private JButton m_stop = new JButton("Stop");
	private JButton m_step = new JButton("Step");
	private JButton m_reset = new JButton("Reset");
	
	private JButton m_arun = new JButton("Agent Run");
	private JButton m_astop = new JButton("Agent Stop");
	private JButton m_astep = new JButton("Agent Step");
	
	private final javax.swing.Timer m_statusTimer =
		new javax.swing.Timer(100, new StatusTimerListener());

	private final ConfigureRunDlg m_configRunDlg = 
		new ConfigureRunDlg(this, m_environment);
	
	/**
	 * Main
	 * Can specify a model version
	 * @author ogeorgeon 
	 * @param strModel
	 */
	public static void main(String[] args){
		version=100;
		if (args.length == 1)
		{
			if (args[0].equals("Ernest110")){
				version=110;
			}
			else if (args[0].equals("Ernest104")){
				version=104;
			}
			else if (args[0].equals("Ernest100")){
				version=100;
			}
			else version=100;
		}
		
		m_modelList=new ArrayList<ErnestModel>();
		m_environment= new Environment(m_modelList,version);
		
		new Main();
	}

	/**
	 * Main
	 */
	public Main()
	{
		super(TITLE);
		MAIN_WIN = this;
		// Retrieve preferences
		m_environment.initPreferences();
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		addWindowListener(new CloseHandler());

		/////////////////
		m_envPanel=new EnvironnementPanel(m_modelList,m_environment);
		

		// Initialize the board
		try{ 
			this.init(m_environment.getBoardFileName());
		}
		catch (Exception e){
			JOptionPane.showMessageDialog(this, 
				"Error intializing the board!\n" + 
				e.getClass().toString() + ": " + e.getMessage(),
				"Error!", 
				JOptionPane.ERROR_MESSAGE);
		}

		m_environment.setFrame(this);
		configureMenu();
		
		m_environment.addObserver(this);

		m_stop.setEnabled(true);
		m_run.setEnabled(true);
		m_reset.setEnabled(true);
		m_step.setEnabled(true);
		
		m_astop.setEnabled(false);
		m_arun.setEnabled(false);
		m_astep.setEnabled(false);
		
		m_stop.addActionListener(this);
		m_run.addActionListener(this);
		m_reset.addActionListener(this);
		m_step.addActionListener(this);
		
		m_astop.addActionListener(this);
		m_arun.addActionListener(this);
		m_astep.addActionListener(this);

		m_board = new JPanel(new GridLayout(1, 1));

		getContentPane().setLayout(new BorderLayout());

		getContentPane().add(m_board, BorderLayout.CENTER);
		
		JPanel buttonPanel = new JPanel();

		buttonPanel.addKeyListener(this);

		buttonPanel.add(m_arun);
		buttonPanel.add(m_astop);
		buttonPanel.add(m_astep);
		
		buttonPanel.add(m_reset);
		buttonPanel.add(m_run);
		buttonPanel.add(m_stop);
		buttonPanel.add(m_step);
		JPanel statusPanel = new JPanel(new BorderLayout());
		statusPanel.add(m_statusBar, BorderLayout.CENTER);
		statusPanel.add(buttonPanel, BorderLayout.EAST);
		statusPanel.setBorder(BorderFactory.createRaisedBevelBorder());
		getContentPane().add(statusPanel, BorderLayout.SOUTH);


		//m_statusModel.pushPermStatus("Ready.");
		m_statusBar.setText("Ready.");
		m_statusBar.setPreferredSize(new Dimension(200, m_statusBar.getHeight()));
		m_statusTimer.start();

		m_chooser.setFileFilter(m_fileFilter);
		m_boardChooser.setFileFilter(m_fileFilter);
		//m_pictureChooser.setFileFilter(m_fileFilter);

		update(null, null);
		pack();
		setVisible(true);
		addKeyListener(this);
		setFocusable(true);
	}
	
	
	
	
	
	/**
	 * Initialize the grid from a board file
	 * @author mcohen
	 * @author ogeorgeon add wall and internal state panel to the grid
	 */
	public void init(String f) throws Exception{
		int l_w;
		int l_h;
		int l_dirtyCount = 0;
		int l_x = -1;
		int l_y = -1;
		
		BufferedReader br = null;
		
		m_modelList.clear();
		try{

			br = new BufferedReader(new FileReader(f));
			List<String> lines = new ArrayList<String>();
			String line = "";
			while ((line = br.readLine()) != null){ 
				line = line.trim();
				if (line.length() != 0)
					lines.add(line); 
			}
			
			m_h = lines.size();
			m_w = (lines.get(0).toString().length() + 1) / 2;
			
			m_environment.init(m_w, m_h);

			if (m_h <= 0 || m_w <= 0)
				throw new IllegalStateException("Invalid width or height!");
			
			int y = 0;

			for (Iterator i = lines.iterator(); i.hasNext(); ){
				
				line = (String)i.next();
				if (((line.length() + 1) / 2) != m_w)
					throw new 
						IllegalStateException("Width must be consistent!");

				String[] square = line.split(" ");
				
				for (int x = 0; x < m_w; x++){
					m_environment.m_blocks[x][m_h-1-y]=m_environment.empty;
					// mauve fish
					if (square[x].equals("*"))
					{
						l_dirtyCount++;
						
						
						m_environment.m_blocks[x][m_h-1-y]=m_environment.fish;
					}
					
					// Agent up
					if (square[x].equalsIgnoreCase("^"))
					{
						int index=m_modelList.size();
						
						if (version==110) m_modelList.add(new Ernest110Model(index));
						else if (version==104) m_modelList.add(new Ernest104Model(index));
						else              m_modelList.add(new Ernest100Model(index));
						m_modelList.get(index).init(m_w, m_h);
						m_modelList.get(index).setFrame(this);

						m_modelList.get(index).mPosition.x = x;
						m_modelList.get(index).mPosition.y = m_h-1 - y;
						m_modelList.get(index).mPosition.z = 0;
						m_modelList.get(index).mOrientation.x = 0;
						m_modelList.get(index).mOrientation.y = 0;
						m_modelList.get(index).mOrientation.z = (float) Math.PI/2;
						
						m_modelList.get(index).setEnvironnement(m_environment);
					}
					// Agent right
					if (square[x].equalsIgnoreCase(">")){
						int index=m_modelList.size();
						
						if (version==110) m_modelList.add(new Ernest110Model(index));
						else if (version==104) m_modelList.add(new Ernest104Model(index));
						else              m_modelList.add(new Ernest100Model(index));
						m_modelList.get(index).init(m_w, m_h);
						m_modelList.get(index).setFrame(this);

						m_modelList.get(index).mPosition.x = x;
						m_modelList.get(index).mPosition.y = m_h-1 - y;
						m_modelList.get(index).mPosition.z = 0;
						m_modelList.get(index).mOrientation.x = 0;
						m_modelList.get(index).mOrientation.y = 0;
						m_modelList.get(index).mOrientation.z = 0;
						
						m_modelList.get(index).setEnvironnement(m_environment);
					}
					
					// Agent down
					if (square[x].equalsIgnoreCase("v")){
						
						int index=m_modelList.size();
						if (version==110) m_modelList.add(new Ernest110Model(index));
						else if (version==104) m_modelList.add(new Ernest104Model(index));
						else              m_modelList.add(new Ernest100Model(index));
						m_modelList.get(index).init(m_w, m_h);
						m_modelList.get(index).setFrame(this);
						
						m_modelList.get(index).mPosition.x = x;
						m_modelList.get(index).mPosition.y = m_h-1 - y;
						m_modelList.get(index).mPosition.z = 0;
						m_modelList.get(index).mOrientation.x = 0;
						m_modelList.get(index).mOrientation.y = 0;
						m_modelList.get(index).mOrientation.z = (float) -Math.PI/2;
						
						m_modelList.get(index).setEnvironnement(m_environment);
					}
					// Agent left
					if (square[x].equalsIgnoreCase("<"))
					{
						int index=m_modelList.size();
						
						if (version==110) m_modelList.add(new Ernest110Model(index));
						else if (version==104) m_modelList.add(new Ernest104Model(index));
						else              m_modelList.add(new Ernest100Model(index));
						m_modelList.get(index).init(m_w, m_h);
						m_modelList.get(index).setFrame(this);

						m_modelList.get(index).mPosition.x = x;
						m_modelList.get(index).mPosition.y = m_h-1 - y;
						m_modelList.get(index).mPosition.z = 0;
						m_modelList.get(index).mOrientation.x = 0;
						m_modelList.get(index).mOrientation.y = 0;
						m_modelList.get(index).mOrientation.z = (float) Math.PI;
						
						m_modelList.get(index).setEnvironnement(m_environment);
					}
					
					if (Character.isLetter(square[x].toCharArray()[0]))
					{
						int code = 'a';
						code = square[x].toCharArray()[0] - code;
						// Agent on target
						if (square[x].equalsIgnoreCase("x"))
						{
							l_dirtyCount++;
							l_x = x;
							l_y = y;	
						}
						// Wall
						else if (square[x].equalsIgnoreCase("w")
							 ||  square[x].equalsIgnoreCase("i")
							 ||  square[x].equalsIgnoreCase("j")){
							
							m_environment.m_blocks[x][m_h-1-y]=m_environment.wall;
						}
						else{
							
							if (square[x].equalsIgnoreCase("g")){
								m_environment.m_blocks[x][m_h-1-y]=m_environment.wall2;
							}
							else if (square[x].equalsIgnoreCase("h")){
								m_environment.m_blocks[x][m_h-1-y]=m_environment.wall3;	
							}
							else m_environment.m_blocks[x][m_h-1-y]=m_environment.empty;
						}
					}
					// Singular dirty square
					if (Character.isDigit(square[x].toCharArray()[0]))
					{
						
						switch (Integer.parseInt(square[x]) ){
						case 2: m_environment.m_blocks[x][m_h-1-y]=m_environment.alga4; break;
						case 3: m_environment.m_blocks[x][m_h-1-y]=m_environment.alga5; break;
						case 4: m_environment.m_blocks[x][m_h-1-y]=m_environment.alga1; break;
						case 5: m_environment.m_blocks[x][m_h-1-y]=m_environment.alga2; break;
						case 9: m_environment.m_blocks[x][m_h-1-y]=m_environment.alga3; break;
						default: break;
						}
					}
				}
				y++;
				
			}
			
			if (m_modelList.size()<=0)
				throw new 
					IllegalStateException("error 404 : Agents not found!");

			//setChanged();
			//notifyObservers2();
			
			m_environment.setDisplay(m_modelList.size()-1);
		}
		catch (Exception e)
		{
			throw e;
		}
		finally
		{
			try { br.close(); } catch (Exception e) {}
		}	
		
		// start simulation
		Thread agentThread = null;
		System.out.println("initialized ") ;
		m_environment.setStop();
		agentThread = new Thread(getErnestView());
		agentThread.start();
		m_statusBar.setText("initialized");

	}
	
	
	
	
	/**
	 * Performs actions
	 */
	public void actionPerformed(ActionEvent e)
	{
		m_modelList.get(0).setEventThread(Thread.currentThread());

		// Run the simulation ******
		if (e.getSource() == m_run){
			System.out.println("Run Ernest ") ;
			m_environment.setRun();
			m_statusBar.setText("Playing");
			m_step.setEnabled(false);
			stopAll=false;
		}
		
		// Run selected agent ******
		else if (e.getSource() == m_arun){
			System.out.println("Run Ernest "+ m_environment.identDisplay+" ") ;
			m_modelList.get(m_environment.identDisplay).run=true;
			stopAll=false;
		}
		
		// Stop the simulation *****
		else if (e.getSource() == m_stop){
			m_environment.setStop();
			m_step.setEnabled(true);
			stopAll=true;
			
			m_statusBar.setText("Pause");			
		}
		
		// Stop selected agent ******
		else if (e.getSource() == m_astop){
			System.out.println("Stop Ernest "+ m_environment.identDisplay+" ") ;
			m_modelList.get(m_environment.identDisplay).run=false;
			
			stopAll=true;
			for (int i=0;i<m_modelList.size();i++){
				if (m_modelList.get(i).run) stopAll=false;
			}
		}
		
		// Run one simulation step *****
		else if (e.getSource() == m_step){
			m_environment.setStep();
			m_environment.setStop();
			System.out.println("Simulation Step") ;
			m_statusBar.setText("Simulation Step");
		}
		
		// Run one selected agent step *****
		else if (e.getSource() == m_astep){
			m_modelList.get(m_environment.identDisplay).step=true;
			//m_environment.setStop();
			System.out.println("Agent "+m_environment.identDisplay+" Step") ;
			m_statusBar.setText("Agent "+m_environment.identDisplay+" Step");
		}
		
		
		// Reset the board ******
		else if (e.getSource() == m_reset){
			m_statusBar.setText("Ready");
			m_modelList.get(0).setCounter(0);
			stopAll=true;
			try
			{ 
				this.init(m_environment.getBoardFileName());
				m_step.setEnabled(false);
				this.repaint();
			}
			catch (Exception ex)
			{
				JOptionPane.showMessageDialog(this, 
					"Error while initializing the board! (Check board file)\n" + 
					e.getClass().toString() + ": " + ex.getMessage(),
					"Error!", 
					JOptionPane.ERROR_MESSAGE);
			}
			m_step.setEnabled(false);
		}
		
		// run one agent
		
		
		
		
		else if (e.getSource() == m_exit)
		{
			System.exit(0);
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
					m_environment.setBoardFileName(boardFile.getAbsolutePath());
					this.init(boardFile.getAbsolutePath()); 
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
		else if (e.getSource() == m_configureRun)
		{
			m_configRunDlg.setVisible(true);
		}
		else if (e.getSource() == m_speakAloud)
		{
			m_modelList.get(0).setSpeakAloud(m_speakAloud.isSelected());
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
		m_modelList.get(0).putPreferences();
		
		
		// enable or disable buttons
		m_step.setEnabled(stopAll);
		m_arun.setEnabled(!m_modelList.get(m_environment.identDisplay).run);
		m_astop.setEnabled( m_modelList.get(m_environment.identDisplay).run);
		m_astep.setEnabled(!m_modelList.get(m_environment.identDisplay).run);
		
	}

	public void update(Observable o, Object arg)
	{
		if (m_modelList.get(0).isAgentStopped())
		{
			m_file.setEnabled(true);
			m_options.setEnabled(true);
			m_help.setEnabled(true);
			//m_statusModel.pushPermStatus("Pause");

			setTitle(TITLE + " - " + m_modelList.get(0).getVersion());
		}
		else
		{
			m_file.setEnabled(false);
			m_options.setEnabled(false);
			m_help.setEnabled(false);

			//m_statusModel.pushPermStatus("Playing");
		}

		drawGrid();
		getContentPane().validate();
		repaint();
		this.repaint();
	}
	/**
	 * Update all the squares in the grid from the model
	 */
	public void drawGrid()
	{
		resizeGrid();
		
		// handle mouse events from continuous environment
		int c= m_envPanel.getClicked();
		if (c == 1){
			int id=m_environment.agentId(m_envPanel.m_FclickX, m_envPanel.m_FclickY);
			if (id==-1){
				
				if (m_environment.isWall(m_envPanel.m_clickX,m_h-1-m_envPanel.m_clickY)){
					m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.empty);
					m_environment.traceUserEvent("remove_wall", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
				}
				else{
					m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.wall);
					m_environment.traceUserEvent("add_wall", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
				}
			}
			else{
				m_environment.setDisplay(id);
				m_statusBar.setText("Agent "+m_environment.identDisplay);
				m_arun.setEnabled(!m_modelList.get(m_environment.identDisplay).run);
				m_astop.setEnabled( m_modelList.get(m_environment.identDisplay).run);
				m_astep.setEnabled(!m_modelList.get(m_environment.identDisplay).run);
			}
		}
		if (c == 3)
		{
			if (m_environment.isAlga(m_envPanel.m_clickX,m_h-1-m_envPanel.m_clickY))
			{
				m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.empty);
				m_environment.traceUserEvent("remove_water", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
			}
			else
			{
				m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.alga1);
				m_environment.traceUserEvent("add_water", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
			}
		}
		if (c == 2)
		{
			if (m_environment.isFood(m_envPanel.m_clickX,m_h-1-m_envPanel.m_clickY))
			{
				m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.empty);
				m_environment.traceUserEvent("remove_food", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
			}
			else
			{
				m_environment.setBlock(m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY, Model.fish);
				m_environment.traceUserEvent("add_food", m_envPanel.m_clickX, m_h-1-m_envPanel.m_clickY);
			}
		}
		
		m_envPanel.repaint();
		
	}

	/**
	 * Creates the grid board if it does not exist or if the grid's size has changed.
	 * @author mchohen
	 */
	private void resizeGrid()
	{
		m_envPanel.setPreferredSize(new Dimension(40*m_envPanel.m_w,40*m_envPanel.m_h));
		m_board.add(m_envPanel);
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
		m_exit.setMnemonic(KeyEvent.VK_X);
		
		m_options.setMnemonic(KeyEvent.VK_O);
		m_configureRun.setMnemonic(KeyEvent.VK_R);
		m_loadBoard.setMnemonic(KeyEvent.VK_L);

		// file menu...
		m_file.add(m_exit);
		
		m_exit.addActionListener(this);

		// options menu...
		m_options.add(m_configureRun);
		m_options.add(m_loadBoard);
		m_options.add(m_speakAloud);

		m_loadBoard.addActionListener(this);
		m_configureRun.addActionListener(this);

		m_speakAloud.setSelected(m_modelList.get(0).getSpeakAloud());
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
			//m_statusBar.setText(m_statusModel.getNextStatus());
		}
	}

	private class CloseHandler extends WindowAdapter
	{
		public void windowClosing(WindowEvent e){
			m_statusTimer.stop(); 
			if (m_ernest != null) m_ernest.close();
		}
	}
	
	/**
	 * Loads the Ernest execution class  
	 * @author ogeorgeon 
	 */
	private ErnestView getErnestView()
	{
		try
		{
			if (m_ernest == null)
				m_ernest = new ErnestView(m_modelList,this);		
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

	public void keyTyped(KeyEvent e) {
	}

	public void keyPressed(KeyEvent e) {
		/* char c = e.getKeyChar();
		 System.out.println(c);
		 int i=m_environment.identDisplay;
		 
		 switch(c){
		 case '+': m_environment.setDisplay(i+1);
		 		   break;
		 case '-': m_environment.setDisplay(i-1);
		   		   break;
		 default:  break;
		 }*/
	}

	public void keyReleased(KeyEvent e) 
	{
	}
}
