

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

    private static ErnestModel m_model;// 			    = new Ernest100Model();
 	//private final StatusModel m_statusModel         = new StatusModel();
    
    private ArrayList<ErnestModel> modelList;
    private int version;
	
	private HelpFrames m_Helpframe;
	private ErnestView m_ernest;
	private EnvSquare[][] m_grid = null;

	private JPanel m_board;
	
	/////////////////////////////////////////////////////////////////////////////////////////
	// tactile properties
	public static final int EMPTY         = 0;
	public static final int SMOOTH        = 1;
	public static final int FOOD          = 2;
	public static final int HARD		  = 3;
	
	// visual properties
	public static final Color FIELD_COLOR = Color.white;
	public static final Color WALL1       = new Color(  0,128,  0);
	public static final Color WALL2       = new Color(  0,230, 92);
	public static final Color WALL3       = new Color(  0,230,161);
	public static final Color ALGA1       = new Color(115,230,  0);
	public static final Color ALGA2       = new Color( 46,230,  0);
	public static final Color ALGA3       = new Color(  0,230,230);
	public static final Color ALGA4       = new Color(230,207,  0);
	public static final Color ALGA5       = new Color(184,230,  0);
	public static final Color FISH1       = new Color(150,128,255);
	
	// objects
	public static Block empty=new Block(EMPTY, FIELD_COLOR,"empty");
	public static Block wall =new Block(HARD , WALL1,"wall1");
	public Block wall2=new Block(HARD , WALL2,"wall2");
	public Block wall3=new Block(HARD , WALL3,"wall3");
	public static Block alga1=new Block(SMOOTH,ALGA1,"alga1");
	public Block alga2=new Block(SMOOTH,ALGA2,"alga2");
	public Block alga3=new Block(SMOOTH,ALGA3,"alga3");
	public Block alga4=new Block(SMOOTH,ALGA4,"alga4");
	public Block alga5=new Block(SMOOTH,ALGA5,"alga5");
	public static Block fish =new Block(FOOD  ,FISH1,"fish");
	
	// trap objects
	public Block green_fish=new Block(FOOD  ,WALL1,"green_fish");
	public Block mauve_wall=new Block(HARD  ,FISH1,"mauve_wall");
	public Block invisible_wall=new Block(HARD,FIELD_COLOR,"invisible");
	
	
	private Environnement m_env;
	public Block[][] m_blocks;
	private int[][] m_anim;
	
	private boolean m_halt = true;
	protected int m_w;
	protected int m_h;
	
	
	private final JLabel m_statusBar = new JLabel();
	private JButton m_run = new JButton("Run");
	private JButton m_stop = new JButton("Stop");
	private JButton m_step = new JButton("Step");
	private JButton m_reset = new JButton("Reset");
	private final javax.swing.Timer m_statusTimer =
		new javax.swing.Timer(100, new StatusTimerListener());

	private final ConfigureRunDlg m_configRunDlg = 
		new ConfigureRunDlg(this, m_model);

	/**
	 * Main
	 * Can specify a model version
	 * @author ogeorgeon 
	 * @param strModel
	 */
	public static void main(String[] args)
	{
		if (args.length == 1)
		{
			if (args[0].equals("Ernest104")){
				m_model = new Ernest104Model();
			}
			if (args[0].equals("Ernest100")){
				m_model = new Ernest100Model();
			}
		}
		else{
			m_model = new Ernest100Model();
		}
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
		m_model.initPreferences();
		setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE);
		addWindowListener(new CloseHandler());


		/////////////////
		m_env=new Environnement(m_model);
		m_model.setFrame(this);
		configureMenu();
		
		
		// Initialize the board
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

		m_model.addObserver(this);

		m_stop.setEnabled(false);
		m_run.setEnabled(false);
		m_reset.setEnabled(true);
		m_step.setEnabled(false);
		m_stop.addActionListener(this);
		m_run.addActionListener(this);
		m_reset.addActionListener(this);
		m_step.addActionListener(this);

		m_board = new JPanel(new GridLayout(1, 1));

		getContentPane().setLayout(new BorderLayout());

		getContentPane().add(m_board, BorderLayout.CENTER);
		
		JPanel buttonPanel = new JPanel();

		buttonPanel.addKeyListener(this);

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
	public void init(String f) throws Exception
	{
		int l_w; // = m_w;
		int l_h; // = m_h;
		int l_dirtyCount = 0;
		int l_x = -1;
		int l_y = -1;

		//mRotation.z = (float) Math.PI/2;
		
		BufferedReader br = null;
		try
		{
			br = new BufferedReader(new FileReader(f));
			List<String> lines = new ArrayList<String>();
			String line = "";
			while ((line = br.readLine()) != null)
			{ 
				line = line.trim();
				if (line.length() != 0)
					lines.add(line); 
			}

			l_h = lines.size();
			l_w = (lines.get(0).toString().length() + 1) / 2;

			if (l_h <= 0 || l_w <= 0)
				throw new IllegalStateException("Invalid width or height!");

			m_anim  = new int[l_w][l_h];			
			m_blocks= new Block[m_w][m_h];
			
			int y = 0;
			for (Iterator i = lines.iterator(); i.hasNext(); )
			{
				line = (String)i.next();
				if (((line.length() + 1) / 2) != l_w)
					throw new 
						IllegalStateException("Width must be consistent!");

				String[] square = line.split(" ");

				for (int x = 0; x < l_w; x++)
				{
					m_blocks[x][y]=empty;
					
					// mauve fish
					if (square[x].equals("*"))
					{
						l_dirtyCount++;
						
						
						m_blocks[x][y]=fish;
					}
					
					// Agent up
					else if (square[x].equalsIgnoreCase("^"))
					{
						l_x = x;
						l_y = y;
						m_model.mPosition.x = x;
						m_model.mPosition.y = m_h - y;
						m_model.mPosition.z = 0;
						m_model.mOrientation.x = 0;
						m_model.mOrientation.y = 0;
						m_model.mOrientation.z = (float) Math.PI/2;
					}
					// Agent right
					else if (square[x].equalsIgnoreCase(">"))
					{
						l_x = x;
						l_y = y;	
						m_model.mPosition.x = x;
						m_model.mPosition.y = m_h - y;
						m_model.mPosition.z = 0;
						m_model.mOrientation.x = 0;
						m_model.mOrientation.y = 0;
						m_model.mOrientation.z = 0;
					}
					// Agent down
					else if (square[x].equalsIgnoreCase("v"))
					{
						l_x = x;
						l_y = y;	
						m_model.mPosition.x = x;
						m_model.mPosition.y = m_h - y;
						m_model.mPosition.z = 0;
						m_model.mOrientation.x = 0;
						m_model.mOrientation.y = 0;
						m_model.mOrientation.z = (float) - Math.PI/2;
					}
					// Agent up
					else if (square[x].equalsIgnoreCase("<"))
					{
						l_x = x;
						l_y = y;	
						m_model.mPosition.x = x;
						m_model.mPosition.y = m_h - y;
						m_model.mPosition.z = 0;
						m_model.mOrientation.x = 0;
						m_model.mOrientation.y = 0;
						m_model.mOrientation.z = (float) Math.PI;
					}
					else if (Character.isLetter(square[x].toCharArray()[0]))
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
							
							m_blocks[x][y]=wall;
						}
						else{
							
							if (square[x].equalsIgnoreCase("g")){
								m_blocks[x][y]=wall2;
							}
							else if (square[x].equalsIgnoreCase("h")){
									m_blocks[x][y]=wall3;	
							}
							else m_blocks[x][y]=empty;
						}
					}
					// Singular dirty square
					else if (Character.isDigit(square[x].toCharArray()[0]))
					{
						
						switch (Integer.parseInt(square[x]) ){
						case 2: m_blocks[x][y]=alga4; break;
						case 3: m_blocks[x][y]=alga5; break;
						case 4: m_blocks[x][y]=alga1; break;
						case 5: m_blocks[x][y]=alga2; break;
						case 9: m_blocks[x][y]=alga3; break;
						default: break;
						}
					}
				}
				y++;
			}

			if (l_x == -1 || l_y == -1)
				throw new 
					IllegalStateException("Agent location not specified!");

			//m_model.m_x = l_x;
			//m_model.m_y = l_y;
			m_w = l_w;
			m_h = l_h;

			//setChanged();
			//notifyObservers2();
		}
		catch (Exception e)
		{
			throw e;
		}
		finally
		{
			try { br.close(); } catch (Exception e) {}
		}
	}
	
	
	
	
	/**
	 * Performs actions
	 */
	public void actionPerformed(ActionEvent e)
	{
		m_model.setEventThread(Thread.currentThread());

		// Run the agent ******
		
		if (e.getSource() == m_run)
		{
			m_model.startAgent();
			Thread agentThread = null;
			System.out.println("Run Ernest ") ;
			agentThread = new Thread(getErnestView());
			agentThread.start();
			m_statusBar.setText("Playing");
		}
		
		// Stop the agent *****
		
		else if (e.getSource() == m_stop)
		{
			m_model.save();
			m_model.haltAgent();
			m_step.setEnabled(true);
			m_statusBar.setText("Pause");			
		}
		
		// Run one step *****
		
		else if (e.getSource() == m_step)
		{
			m_statusBar.setText("Step");
		}
		
		// Reset the board ******
		else if (e.getSource() == m_reset)

		{
			m_statusBar.setText("Ready");
			m_model.setCounter(0);
			try
			{ 
				m_model.init(m_model.getBoardFileName()); 
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
		else if (e.getSource() == m_configureRun)
		{
			m_configRunDlg.setVisible(true);
		}
		else if (e.getSource() == m_speakAloud)
		{
			m_model.setSpeakAloud(m_speakAloud.isSelected());
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
			m_file.setEnabled(true);
			m_options.setEnabled(true);
			m_help.setEnabled(true);
			//m_statusModel.pushPermStatus("Pause");

			m_run.setEnabled(true);
			m_reset.setEnabled(true);
			m_stop.setEnabled(false);
			//m_step.setEnabled(false);
			setTitle(TITLE + " - " + m_model.getVersion());
		}
		else
		{
			m_file.setEnabled(false);
			m_options.setEnabled(false);
			m_help.setEnabled(false);
			m_run.setEnabled(false);
			m_reset.setEnabled(false);
			m_step.setEnabled(false);

			m_stop.setEnabled(true);
			m_stop.setSelected(true);

			//m_statusModel.pushPermStatus("Playing");
		}

		drawGrid();
		getContentPane().validate();
		repaint();
		m_env.repaint();
	}
	/**
	 * Update all the squares in the grid from the model
	 */
	public void drawGrid()
	{
		resizeGrid();
		
		// handle mouse events from continuous environment
		int c= m_env.getClicked();
		if (c == 1)
		{
			if (m_model.isWall(m_env.m_clickX,m_env.m_clickY))
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.empty);
				m_model.traceUserEvent("remove_wall", m_env.m_clickX, m_env.m_clickY);
			}
			else
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.wall);
				m_model.traceUserEvent("add_wall", m_env.m_clickX, m_env.m_clickY);
			}
		}
		if (c == 3)
		{
			if (m_model.isAlga(m_env.m_clickX,m_env.m_clickY))
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.empty);
				m_model.traceUserEvent("remove_water", m_env.m_clickX, m_env.m_clickY);
			}
			else
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.alga1);
				m_model.traceUserEvent("add_water", m_env.m_clickX, m_env.m_clickY);
			}
		}
		if (c == 2)
		{
			if (m_model.isFood(m_env.m_clickX,m_env.m_clickY))
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.empty);
				m_model.traceUserEvent("remove_food", m_env.m_clickX, m_env.m_clickY);
			}
			else
			{
				m_model.setBlock(m_env.m_clickX, m_env.m_clickY, Model.fish);
				m_model.traceUserEvent("add_food", m_env.m_clickX, m_env.m_clickY);
			}
		}
		if (c == 4)
			m_model.toggleNight();
		
		m_env.repaint();
		
	}

	/**
	 * Creates the grid board if it does not exist or if the grid's size has changed.
	 * @author mchohen
	 */
	private void resizeGrid()
	{
//<<<<<<< .mine
//	
//		if (m_grid == null || 
//			m_grid.length != m_model.getHeight() ||
//			m_grid[0].length != m_model.getWidth())
//		{
//			m_board.removeAll();
//			m_board.setBackground(Model.WALL_COLOR);
//			m_board.setLayout(new GridLayout(m_model.getHeight(),  m_model.getWidth()));
//			m_grid = new EnvSquare[m_model.getHeight()][m_model.getWidth()];
//			
//			for (int y = 0; y < m_model.getHeight(); y++)
//			{
//				for (int x = 0; x < m_model.getWidth(); x++)
//				{
//                	m_grid[y][x] = new EnvSquare(x, y, m_model);
//                	m_grid[y][x].setPreferredSize(new Dimension(37,37));
//                	m_board.add(m_grid[y][x]);
//				}
//			}
//			//m_env=new Environnement(m_model);
//			//m_env.setPreferredSize(new Dimension(400,400));
//			//m_board.add(m_env);
//		}
//=======
		m_env.setPreferredSize(new Dimension(40*m_env.m_w,40*m_env.m_h));
		m_board.add(m_env);
//>>>>>>> .r73
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
			//m_statusBar.setText(m_statusModel.getNextStatus());
		}
	}

	private class CloseHandler extends WindowAdapter
	{
		public void windowClosing(WindowEvent e)
		{ m_statusTimer.stop(); }
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

	public void keyTyped(KeyEvent e) {
	}

	public void keyPressed(KeyEvent e) {
	}

	public void keyReleased(KeyEvent e) 
	{
	}
}
