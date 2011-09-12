

import java.util.*;
import java.util.prefs.Preferences;
import java.awt.Color;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Image;
import java.awt.RenderingHints;
import java.awt.geom.AffineTransform;
import java.io.*;

import javax.swing.ImageIcon;
import javax.swing.JOptionPane;
import javax.swing.SwingUtilities;
import java.lang.reflect.InvocationTargetException;

import jess.JessException;


/**
 * Represent the Environment reacting to the agent's actions
 * @author mcohen
 * @author ogeorgeon Add wall on the grid
 */

public class Model extends Observable 
{
	public static final long serialVersionUID = 1;

	public static final int AGENTUNDEFINED = 0;
	public static final int JESSFILE       = 1;
	public static final int SOARFILE       = 2;
	public static final int HUMANLOGFILE   = 3;
	public static final int ERNEST         = 4;
	
	public static final int EMPTY         = 0;
	public static final int DIRTY         = 1;
	public static final int FOOD          = 2;
	public static final int WALL          = 1; // default wall 
	public static final int WALL_INFORMATION = 10;
	public static final int WALL_INFORMATION2 = 11;

	public static final int ANIM_NO       = 0;
	public static final int ANIM_BUMP     = 1;
	public static final int ANIM_RUB      = 2;
	public static final int ANIM_TOUCH    = 3;
	
	public static final Color FIELD_COLOR = Color.white; //new Color(150, 255, 150);
	public static final Color WALL_COLOR  = new Color(0, 128, 0); // Color.getHSBColor(1/3f, 1f, 0.5f)
	public static final Color WATER_COLOR = new Color(150, 128, 255); // Color.getHSBColor(1/3f, 1f, 0.5f)
	//public static final Color WATER_COLOR = new Color(0,0,255); // Color.getHSBColor(1/3f, 1f, 0.5f)
	public static final Color FOOD_COLOR  = new Color(227, 124, 255); // Color.getHSBColor(1/3f, 1f, 0.5f)
	public static final Color THIRSTY_HUNGRY_COLOR = new Color(190, 126, 255);

	public static final int INIT_W         = 4;
	public static final int INIT_H         = 4;
	public static final int INIT_X         = 0;
	public static final int INIT_Y         = 0;
	public static final int INIT_DIRTY     = 10;
	public static final int INIT_STEPS     = 100;
	public static final int INIT_DELAY     = 500;
	public static final int ORIENTATION_UP    = 0; //(Orientation is clockwise)
	public static final int ORIENTATION_RIGHT = 90;
	public static final int ORIENTATION_DOWN  = 180;
	public static final int ORIENTATION_LEFT  = 270;
	private int m_orientationStep  = 45;
	protected void setOrientationStep(int step) { m_orientationStep = step;}
	protected int getOrientationStep() { return m_orientationStep;}
	
	public static final String INIT_PICTURE    = "vacuum.gif";
	
	private static final String PREF_W = "pref_w";
	private static final String PREF_H = "pref_h";
	private static final String PREF_X = "pref_x";
	private static final String PREF_Y = "pref_y";
	private static final String PREF_DIRTY = "pref_dirty";
	private static final String PREF_STEPS = "pref_steps";
	private static final String PREF_DELAY = "pref_delay";
	private static final String PREF_RANDOMBOARD = "pref_randomBoard";
	private static final String PREF_BOARDFILE = "pref_boardFile";
	private static final String PREF_PICTUREFILE = "pref_pictureFile";
	private static final String PREF_SPEAKALOUD = "pref_speakAloud";
	private static final String PREF_AGENTFILE = "pref_agentFile";
	private static final String PREF_AGENTTYPE = "pref_agentType";
	private static final String PREF_AGENTSHORTFILE = "pref_agentShortFile";
		
	protected int m_w;
	protected int m_h;
	private int m_dirtyCount;
	private int m_totalSteps;
	private int m_delay;
	private int m_informationX = 100;
	private int m_informationY = 100;
	protected int m_counter = 0;

	// A single agent in the environment.
	protected float m_x;
	protected float m_y;
	public int m_orientation = 1; 
	public String m_schema = "";
	protected double m_eyeOrientation = 0;
	/** The angular orientation of Ernest. (clockwise)*/
	protected double m_orientationAngle = 0;

	private int[][] m_dirty;
	private int[][] m_wall;
	private int[][] m_anim;

	// Maik Friedrich
	private String m_boardFileName = "";

	private String BoardTempFile;
	private int lastFinalScore;

	private boolean m_halt = true;
	private int m_stepCount = m_totalSteps;
	private int m_score = 0;

	private boolean m_bPenalizeForMovement = false;

	private static final Random m_rand = new Random();

	private final Runnable m_thread = new NotifyThread();
	private Runnable m_mainThread;
	private Runnable m_eventThread;
	
	private boolean m_bRandomBoard    = true;
	private boolean m_bSpeakAloud     = true;
	private boolean m_bInternalState  = false;
	private boolean m_status          = false; 
	

	private boolean m_night = false;

	protected EnvironnementFrame m_env;

	
	public Model()
	{
		m_mainThread = Thread.currentThread();
		

	}
	public void setEnvironnement(EnvironnementFrame env){
		m_env=env;
	}
	
	// save the actual ernest
	public void save(){
	}

	/**
	 * Initialize the grid randomly
	 * @author mcohen
	 * @author ogeorgeon Initialize no wall
	 */
	public void init()
	{
		m_dirty = new int[m_w][m_h];
		m_wall  = new int[m_w][m_h];
		m_anim  = new int[m_w][m_h];
		m_score = 0;
		m_stepCount = m_totalSteps;
		m_halt = true;
		
		m_orientation = ORIENTATION_UP;
		m_orientationAngle = 0;
		m_eyeOrientation = 0;
		
		for (int i = 0; i < m_dirtyCount; i++)
		{
			int dx = m_rand.nextInt(m_w);
			int dy = m_rand.nextInt(m_h);
			while (m_dirty[dx][dy] == DIRTY)
			{
				dx = m_rand.nextInt(m_w);
				dy = m_rand.nextInt(m_h);
			}
			m_dirty[dx][dy] = DIRTY;
		}

		setChanged();
		notifyObservers2();
	}

	/**
	 * @return The version of Ernest
	 */
	public String getVersion()
	{
		return "Ernest";
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

		m_orientation = ORIENTATION_UP;
		m_orientationAngle = 0;
		m_eyeOrientation = 0;
		
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

			m_dirty = new int[l_w][l_h];
			m_wall  = new int[l_w][l_h]; 
			m_anim  = new int[l_w][l_h];

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
					m_wall[x][y] = EMPTY;
					// Target
					if (square[x].equals("*"))
					{
						m_dirty[x][y] = DIRTY;
						l_dirtyCount++;
					}
					// Target type 2
					if (square[x].equals("+"))
					{
						m_dirty[x][y] = 2;
						l_dirtyCount++;
					}
					// Agent up
					else if (square[x].equalsIgnoreCase("^"))
					{
						m_orientation = ORIENTATION_UP;
						l_x = x;
						l_y = y;	
					}
					// Agent right
					else if (square[x].equalsIgnoreCase(">"))
					{
						m_orientation = ORIENTATION_RIGHT;
						l_x = x;
						l_y = y;	
					}
					// Agent down
					else if (square[x].equalsIgnoreCase("v"))
					{
						m_orientation = ORIENTATION_DOWN;
						l_x = x;
						l_y = y;	
					}
					// Agent up
					else if (square[x].equalsIgnoreCase("<"))
					{
						m_orientation = ORIENTATION_LEFT;
						l_x = x;
						l_y = y;	
					}
					else if (Character.isLetter(square[x].toCharArray()[0]))
					{
						int code = 'a';
						code = square[x].toCharArray()[0] - code;
						// Agent on target
						if (square[x].equalsIgnoreCase("x"))
						{
							m_dirty[x][y] = DIRTY;
							l_dirtyCount++;
							l_x = x;
							l_y = y;	
						}
						// Wall
						else if (square[x].equalsIgnoreCase("w"))
						{
							m_wall[x][y] = WALL;
						}
						// Information square
						//else if (square[x].equalsIgnoreCase("i"))
						//{
						//	m_wall[x][y] = WALL; 
						//	m_informationX = x;
						//	m_informationY = y;
						//}
						// Singular wall
						else
						{
							m_wall[x][y] = code + WALL + 1;
						}
					}
					// Singular dirty square
					else if (Character.isDigit(square[x].toCharArray()[0]))
					{
						m_dirty[x][y] = Integer.parseInt(square[x]) + DIRTY;
					}
				}
				y++;
			}

			m_orientationAngle =  (m_orientation ) * Math.PI/2 / ORIENTATION_RIGHT;

			if (l_x == -1 || l_y == -1)
				throw new 
					IllegalStateException("Agent location not specified!");

			m_x = l_x;
			m_y = l_y;
			m_w = l_w;
			m_h = l_h;

			setChanged();
			notifyObservers2();
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
	 * Sets the parameters of the random board
	 * @author ogeorgeon
	 * This function can be used before calling init();
	 */
	public void setRandomBoardParameters(int w, int h, int dirtyCount)
	{
		if (w < 0)
			throw new IllegalArgumentException("Invalid starting W");
		if (h < 0)
			throw new IllegalArgumentException("Invalid starting h");
		if ( (dirtyCount > (w*h)) || (dirtyCount < 0) )
			throw new IllegalArgumentException
				("Invalid initial number of dirty squares");

		m_w = w;
		m_h = h;
		m_x = m_rand.nextInt(w);
		m_y = m_rand.nextInt(h);
		m_dirtyCount = dirtyCount;
		putPreferences();
	}

	
	/**
	 * return the nearest integer of a float
	 */
	private int cell(float a){
		if (a-(int)a <=0.5) return (int)a;
		else                return (int)a+1;
	}
	
	
	public void setEventThread(Runnable t)
	{ m_eventThread = t; }

	public boolean isDirty()
	{ 
		return m_dirty[Math.round(m_x)][Math.round(m_y)] > EMPTY;
	}

	public boolean isDirty(float x, float y)
	{ 
		return m_dirty[Math.round(x)][Math.round(y)] > EMPTY; 
	}

	public void setDirty(int x, int y, int dirty)
	{ 
			m_dirty[x][y] = dirty;
	}

	public int getDirty(float x, float y)
	{ 
		return m_dirty[Math.round(x)][Math.round(y)]; 
	}

	public boolean isWall(float x, float y)
	{ 
		return 	(m_wall[Math.round(x)][Math.round(y)] != EMPTY); 
	}
	
	public boolean affordSee(float x, float y)
	{
		return 	(m_wall[Math.round(x)][Math.round(y)] != EMPTY); 		
	}

	public void setWall(int x, int y, int wall)
	{ 
		m_wall[x][y] = wall;
	}

	public int getWall(float x, float y)
	{ 
		return 	m_wall[Math.round(x)][Math.round(y)]; 
	}

	public boolean isAgent(int x, int y)
	{
		return (x == Math.round(m_x)) && (y == Math.round(m_y));
	}
	
	public boolean isInformation(int x, int y)
	{ 
		return (m_informationX == x && m_informationY == y);
	}

	public void setCounter(int counter)
	{
		m_counter = counter;
	}
	
	public int getCounter()
	{
		return m_counter;
	}

	/**
	 * Returns the animation value
	 * (For yellow flashing the perceived square)
	 * @author ogeorgeon 
	 */
	public int getAnim(float x, float y)
	{ 
		return m_anim[Math.round(x)][Math.round(y)];
	}

	public void setAnim(float x, float y, int anim)
	{ 
		if (anim == ANIM_BUMP)
			speak("Ouch", false , false);

		m_anim[Math.round(x)][Math.round(y)] = anim;
	}

	/**
	 * getStatus
	 * Returns the status value of a schema enaction for an Ernest model
	 * @author ogeorgeon 
	 */
	public boolean getStatus()
	{
		return m_status;
	}

	public int getDirtyCount()
	{ return m_dirtyCount; }

	public boolean isAgentStopped()
	{ return m_halt; }

	public int getStepCount()
	{ return m_stepCount; }

	public int getDelay()
	{ return m_delay; }

	public float agentX()
	{ return m_x; }

	public float agentY()
	{ return m_y; }

	public int getWidth()
	{ return m_w; }

	public int getHeight()
	{ return m_h; }

	public int getScore()
	{ return m_score; }

	public int getTotalSteps()
	{ return m_totalSteps; }

	public int getCleanSquareCount()
	{
		int count = 0;
		for (int y = 0; y < m_h; y++)
		{
			for (int x = 0; x < m_w; x++)
			{
				if (m_dirty[x][y] != DIRTY)
					count++;
			}
		}
		return count;
	}

	public boolean getRandomBoard()
	{
		return m_bRandomBoard;
	}

	public boolean getSpeakAloud()
	{
		return m_bSpeakAloud;
	}
	public boolean getInternalState()
	{
		return m_bInternalState;
	}

	public void setRandomBoard(boolean b)
	{
		m_bRandomBoard = b;
	}

	public void setSpeakAloud(boolean b)
	{
		m_bSpeakAloud = b;
	}
	public void setInternalState(boolean b)
	{
		m_bInternalState = b;
	}

	/**
	 * Suck a dirty square
	 * @author mcohen
	 * @author ogeorgeon
	 * @return true if the square was dirty, false if the square was clean
	 */
	public boolean suck()
	{
		if (isDirty())
		{
			m_dirty[cell(m_x)][cell(m_y)] = EMPTY;
			setChanged();
			notifyObservers2();
			
			speak("Yummy", true , false);
			sleep(20);

			return true;
		}
		return false;
	}

	/**
	 * Move to the left if there is no wall
	 * @author mcohen
	 * @author ogeorgeon
	 */
	public void left()
	{
		if (m_x > 0) 
		{
			if (!isWall(m_x - 1,m_y) )
				m_x--;
			else
				setAnim(m_x - 1,m_y, ANIM_BUMP); 
		}
		
		if (m_bPenalizeForMovement) m_score--;

		setChanged();
		notifyObservers2();
	}

	/**
	 * Move to the right if there is no wall
	 * @author mcohen
	 * @author ogeorgeon
	 */
	public void right()
	{
		if (m_x < (m_w-1))
		{
			if (!isWall(m_x + 1,m_y))
				m_x++;
			else
				setAnim(m_x + 1,m_y, ANIM_BUMP); 
		}

		if (m_bPenalizeForMovement) m_score--;

		setChanged();
		notifyObservers2();
	}

	/**
	 * Move up if there is no wall
	 * @author mcohen
	 * @author ogeorgeon
	 */
	public void up()
	{
		if (m_y > 0) 
		{
			if  (!isWall(m_x,m_y - 1))
				m_y--;
			else
				setAnim(m_x,m_y - 1, ANIM_BUMP); 
		}
		
		if (m_bPenalizeForMovement) m_score--;

		setChanged();
		notifyObservers2();
	}

	/**
	 * Move down if there is no wall
	 * @author mcohen
	 * @author ogeorgeon
	 */
	public void down()
	{
		if (m_y < (m_h - 1))
		{
			if  (!isWall(m_x,m_y + 1))
				m_y++;
			else 
				setAnim(m_x,m_y + 1, ANIM_BUMP); 
		}

		if (m_bPenalizeForMovement) m_score--;

		setChanged();
		notifyObservers2();
	}
	
	public void setPenalizeForMovement(boolean bPenalize)
	{
		m_bPenalizeForMovement = bPenalize;
		setChanged();
		notifyObservers2();
	}

	public void haltAgent()
	{ 
		m_halt = true; 
		setChanged();
		notifyObservers2();
	}

	public void startAgent()
	{ 
		m_stepCount = m_totalSteps; 
		m_halt = false; 
		setChanged();
		notifyObservers2();
	}

	public void resetStepCount()
	{
		m_stepCount = m_totalSteps; 
		setChanged();
		notifyObservers2();
	}

	public void decStepCount()
	{ 
		m_stepCount--; 
		setChanged();
		// ogeorgeon: Do no refresh the display here so the agent's actions remain visible longer 
		// notifyObservers2();
	}

	public void setTotalSteps(int steps)
	{ 
		m_totalSteps = steps; 
		m_stepCount = steps;
		setChanged();
		notifyObservers2();
	}

	public void setDelay(int delay)
	{ 
		m_delay = delay; 
		setChanged();
		notifyObservers2();
	}

	public void tallyScore()
	{ 
		m_score += getCleanSquareCount();
		setChanged();
		// ogeorgeon: Do no refresh the display here so the agent's actions remain visible longer 
		// notifyObservers2();
	}

	public void resetScore()
	{
		m_score = 0;
		setChanged();
		notifyObservers2();
	}

	/**
	 * Initialize the preferences from Registry
	 * @author ogeorgeon 
	 */
	public void initPreferences()
	{
		Preferences prefs = Preferences.userRoot().node("vacuum");
		
		m_w = prefs.getInt(PREF_W,INIT_W);
		m_h = prefs.getInt(PREF_H,INIT_H);
		m_x = prefs.getInt(PREF_X,INIT_X);
		m_y = prefs.getInt(PREF_Y,INIT_Y);
		m_dirtyCount = prefs.getInt(PREF_DIRTY,INIT_DIRTY);
		m_totalSteps = prefs.getInt(PREF_STEPS,INIT_STEPS);
		m_delay = prefs.getInt(PREF_DELAY,INIT_DELAY);
		m_bRandomBoard = prefs.getBoolean(PREF_RANDOMBOARD, true);
		m_boardFileName = prefs.get(PREF_BOARDFILE, "");
		m_bSpeakAloud = prefs.getBoolean(PREF_SPEAKALOUD, true);
		
	}

	/**
	 * Save the preferences to Registry
	 * @author ogeorgeon 
	 */
	public void putPreferences()
	{
		Preferences prefs = Preferences.userRoot().node("vacuum");
		
		prefs.putInt(PREF_W, m_w);
		prefs.putInt(PREF_H, m_h);
		prefs.putFloat(PREF_X, m_x);
		prefs.putFloat(PREF_Y, m_y);
		prefs.putInt(PREF_DIRTY, m_dirtyCount);
		prefs.putInt(PREF_STEPS, m_totalSteps);
		prefs.putInt(PREF_DELAY, m_delay);
		prefs.putBoolean(PREF_RANDOMBOARD, m_bRandomBoard);
		prefs.put(PREF_BOARDFILE, m_boardFileName);
		prefs.putBoolean(PREF_SPEAKALOUD, m_bSpeakAloud);
	}

	protected void notifyObservers2()
	{
		try
		{
			if ( (m_mainThread != Thread.currentThread()) &&
	 			 (m_eventThread != Thread.currentThread())	)
			{
				SwingUtilities.invokeAndWait(m_thread);
			}
			else
			{
				m_thread.run();
			}
		}
		catch  (InvocationTargetException e)
		{
		  	throw new IllegalStateException("Error notifying observers!");
		}
		catch  (InterruptedException e)
		{
			throw new IllegalStateException("Error notifying view! (Deadlock?)");
		}
	}

	private class NotifyThread implements Runnable
	{
		public void run()
		{ 
			notifyObservers(); 
		}
	}

	public void setBoardFileName(String file) {
		m_boardFileName = file;
	}

	public int getLastFinalScore() {
		return lastFinalScore;
	}

	public void setLastFinalScore(int lastFinalScore) {
		this.lastFinalScore = lastFinalScore;
	}

	public void saveCurrentBoard() {

		if (!this.BoardTempFile.equals("")) {
			try {

				File output = new File(this.BoardTempFile);
				if (output.exists()) {
					output.delete();
					output.createNewFile();
				}
				FileWriter writer = new FileWriter(output);
				for (int y = 0; y < this.getHeight(); y++) {
					for (int x = 0; x < this.getWidth(); x++) {
						if ((this.agentX() == x) && (this.agentY() == y)
								&& this.isDirty(x, y)) {
							writer.write("x ");
						} else if ((this.agentX() == x) && (this.agentY() == y)
								&& !this.isDirty(x, y)) {
							writer.write("v ");
						} else {
							if (this.isDirty(x, y)) {
								writer.write("* ");
							} else {
								writer.write("- ");
							}
						}
					}
					writer.write("\n");
				}

				writer.close();

			} catch (IOException e) {
				e.printStackTrace();
			}
		}
	}

	public void setBoardTempFile(String string) {

		if (System.getProperty("os.name").startsWith("Windows")) {
			this.BoardTempFile = (System.getProperty("user.dir") + "\\" + string);
		} else {
			this.BoardTempFile = (System.getProperty("user.dir") + "/" + string);
		}
	}

	public boolean boardTempExists() {
		if (new File(this.BoardTempFile).exists())
			return true;
		return false;
	}

	public String getBoardTempFile() {
		return BoardTempFile;
	}
	public String getBoardFileName() {
		return m_boardFileName;
	}

	/**************************************
	 * Speak aloud a text
	 * @param wait wait befor proceeding
	 * @param force Force the speak aloud
	 **************************************/
	public void speak(String text, boolean wait,boolean force)
	{

	}
		
	/**
	 * @param x The x coordinate of the square
	 * @param y The y coordinate of the square
	 * @return The background color of a square
	 */
	public Color getBackgroundColor(float x, float y)
	{
		Color backgroundColor = FIELD_COLOR;
		
		if (getDirty(x, y) == DIRTY)
			backgroundColor = WATER_COLOR ;			
		if (getDirty(x, y) == FOOD)
			backgroundColor = FOOD_COLOR;			
		if (getDirty(x, y) > FOOD)
		{
			float hue = getDirty(x, y) / 20.0f;
			backgroundColor = Color.getHSBColor(hue, 1f, 0.9f);
		}
		else if (getWall(x, y) == WALL || getWall(x, y) == WALL_INFORMATION || getWall(x, y) == WALL_INFORMATION2)
			backgroundColor = WALL_COLOR;
		else if (getWall(x, y) > WALL)
		{
			float hue = getWall(x, y) / 20.0f;
			backgroundColor = Color.getHSBColor(hue, 1.0f, 0.9f);
		}
		
		if (getAnim(x,y) == ANIM_BUMP)
			backgroundColor = Color.RED;
		//else if (getAnim(x,y) == ANIM_RUB)
		// 	backgroundColor = Color.PINK;
		else if (getAnim(x,y) == ANIM_TOUCH)
			backgroundColor = Color.YELLOW;
		
		//setAnim(m_x, m_y, ANIM_NO);

		return backgroundColor;
	}
	
	/**
	 * Paint a square
	 */
	public void paint(int x, int y, Graphics g) 
	{
	}
	
	/**
	 * Initialize the agent's picture
	 * To support the agent's rotation, picture file names must end with _up _right _down _left.
	 */
	private ImageIcon m_icon_up; 
	private ImageIcon m_icon_right; 
	private ImageIcon m_icon_down; 
	private ImageIcon m_icon_left; 
	
	public void setPicture(String pictureFileName)
	{
		m_icon_up    = new ImageIcon(pictureFileName);
		if ( pictureFileName.indexOf("_up.") > 0 )
		{
			m_icon_right = new ImageIcon(pictureFileName.replaceFirst("_up.", "_right."));
			m_icon_down  = new ImageIcon(pictureFileName.replaceFirst("_up.", "_down."));
			m_icon_left  = new ImageIcon(pictureFileName.replaceFirst("_up.", "_left."));
		}
		else
		{
			m_icon_right = new ImageIcon(pictureFileName);			
			m_icon_down  = new ImageIcon(pictureFileName);			
			m_icon_left  = new ImageIcon(pictureFileName);			
		}
	}
	
	/**
	 * Paint the agent as an icon.
	 * @param g The graphic object for painting.
	 */
	public void paintAgent(Graphics2D g,int x,int y,double sx,double sy)
	{
		Image img = m_icon_up.getImage();
		if (m_orientation == ORIENTATION_RIGHT)
			img = m_icon_right.getImage();
		if (m_orientation == ORIENTATION_DOWN)
			img = m_icon_down.getImage();
		if (m_orientation == ORIENTATION_LEFT)
			img = m_icon_left.getImage();

		g.drawImage(img, 1, 1, null); // TODO check the position and size
	}

	protected void sleep(int t)
	{
		try
		{ 
			Thread.currentThread().sleep(t);
		}
		catch(InterruptedException ie)
		{}
	}
	
	public boolean isNight()
	{
		return m_night;
	}

	public void toggleNight()
	{
		m_night = !m_night;
	}

	public void paintDream(Graphics2D g,int x,int y,double sx,double sy)
	{
		
	}
}
