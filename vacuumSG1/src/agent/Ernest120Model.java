package agent;

import javax.swing.JFrame;
import javax.vecmath.Matrix3f;
import javax.vecmath.Vector3f;

import memory.ColliculusFrame;
import memory.TactileMapFrame;
import memory.VisualMapFrame;
import memory110.SpaceMemory;
import memory110.SpaceMemoryFrame;
import memory110.SpaceMemorySimulationFrame;

import java.awt.AlphaComposite;
import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.Polygon;
import java.awt.RenderingHints;
import java.awt.Shape;
import java.awt.geom.AffineTransform;
import java.awt.geom.Arc2D;
import java.awt.geom.Area;
import java.awt.geom.CubicCurve2D;
import java.awt.geom.Ellipse2D;
import java.awt.geom.GeneralPath;
import java.awt.geom.Rectangle2D;
import java.util.ArrayList;
import java.util.List;

import ernest.*;
import spas.IAffordance;
import spas.IObservation;
import spas.IPlace;
import spas.Observation;
import imos.IAct;
import spas.LocalSpaceMemory;
import spas.Spas;
import tracing.*;
import utils.ErnestUtils;
import utils.Pair;

/**************************************
 * A Model for a fish that moves in the environment
 * @author ogeorgeon
 **************************************/
public class Ernest120Model extends ErnestModel 
{
    final static float TRANSLATION_FRICTION = .90f; // .95f
    
    final static float ROTATION_FRICTION = .9f; // .95f
    
    final static float SLIPPERY_EFFECT = 4.8f;

    final static String FEEDBACK_TRUE = "t";
    final static String FEEDBACK_FALSE = "f";
    final static String FEEDBACK_BRICK = "b";
    final static String FEEDBACK_ALGA = "a";
    
    private static Color UNANIMATED_COLOR = Color.GRAY;
    
    public SpaceMemory m_SpaceMemory;
    
    private Color[] pixelColor = new Color[Ernest.RESOLUTION_RETINA];
    private Color[][] somatoMapColor = new Color[3][3];
    private Color focusColor = UNANIMATED_COLOR;
    private Color leftColor = UNANIMATED_COLOR;
    private Color rightColor = UNANIMATED_COLOR;
    
    private String m_status = FEEDBACK_TRUE;
    
    private IObservation m_observation = new Observation();
    
    float m_animOrientation = 0;
    float m_animPosition = 0;
    
    Pair<Integer, Color>[] eyeFixation = null;
    /** The values of the pixels */
    private int m_currentLeftPixel   = Ernest.INFINITE;
    private int m_currentRightPixel  = Ernest.INFINITE;
    private int m_previousLeftPixel  = Ernest.INFINITE;
    private int m_previousRightPixel = Ernest.INFINITE;
    
    
    /** The features that are sensed by the distal system. */
    private String m_leftFeature = " ";
    private String m_rightFeature = " ";
    
    /** The intrinsic satisfaction of sensing the current features */
    private int m_satisfaction = 0;
    
    //private JFrame m_simulationFrame;

    /**
     * @param i The agent's numerical id. 
     */
    public Ernest120Model(int i) 
    {
        super(i);
    }
    
    /**
     * Initialize the agent in the grid
     */
    public void init(int w,int h) throws Exception
    {
        // Initialize the model
        super.init(w,h);
        
        // Initialize Ernest's colors 
        for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
            pixelColor[i] = UNANIMATED_COLOR;
        
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++)
                somatoMapColor[i][j] = UNANIMATED_COLOR;
        
        focusColor = UNANIMATED_COLOR;
        leftColor =  UNANIMATED_COLOR;
        rightColor =  UNANIMATED_COLOR;

        setChanged();
        notifyObservers2();  
        
        m_SpaceMemory=new SpaceMemory();
    }

    /**
     * @return The version of the Ernest model
     */
    public String getVersion()
    {
        return "Ernest 12.0";
    }
    
    /**
     * Initialize the Ernest agent.
     */
    public void initErnest()
    {
    	// Instantiate Ernest
        m_ernest = new Ernest();
        //m_sensorymotorSystem = new SpatialSensorimotorSystem();
        
        // Initialize the visualization.
		m_SpaceMemory.setModel(this);
		m_eye.setModel(this);
		
        // Only trace the first agent.
        
        //if (ident == 8)
        //m_tracer = new XMLStreamTracer("http://macbook-pro-de-olivier-2.local/alite/php/stream/","NKmqGfrDVaTZQDSsgKNazjXd-cG-TZ");
                        
        // Initialize the Ernest === 
        
        // Ernest's inborn primitive interactions

        //m_ernest.setParameters(4, 4);
        m_ernest.setParameters(4, 4);
        m_ernest.setTracer(m_tracer);
        m_ernest.setSensorymotorSystem(new Ernest12SensorimotorSystem());
        //m_ernest.setSensorymotorSystem(new BinarySensorymotorSystem());
        //m_ernest.setFrame(m_simulationFrame);

        // For the small loop
        m_ernest.addInteraction("-", "f",  -1); // Touch empty
        m_ernest.addInteraction("-", "t",  -1); // Touch wall
        m_ernest.addInteraction("-", "b",  -1); // Touch brick
        m_ernest.addInteraction("-", "a",  -1); // Touch alga
        m_ernest.addInteraction("\\","f",  -1); // Touch right empty
        m_ernest.addInteraction("\\","t",  -1); // Touch right wall
        m_ernest.addInteraction("/", "f",  -1); // Touch left empty
        m_ernest.addInteraction("/", "t",  -1); // Touch left wall
        m_ernest.addInteraction(">", "t",   5); // Move
        m_ernest.addInteraction(">", "f",  -10);// Bump
//        m_ernest.addInteraction("<", "t",  -10); // Move backward
//        m_ernest.addInteraction("<", "f",  -10);// Bump backward
        m_ernest.addInteraction(">", "a",  10);//  Move to alga
        m_ernest.addInteraction("v", "t",  -3); // Right
        m_ernest.addInteraction("v", "f",  -3); // Right 
        m_ernest.addInteraction("^", "t",  -3); // Left
        m_ernest.addInteraction("^", "f",  -3); // Left 

        // With vision 
//        m_ernest.addInteraction("-", "f",  -1); // Touch empty
//        m_ernest.addInteraction("-", "t",  -1); // Touch wall
//        m_ernest.addInteraction("-", "b",  -1); // Touch brick
//        m_ernest.addInteraction("-", "a",  -1); // Touch alga
//        m_ernest.addInteraction(">", "t",   0); // 5 Move 0
//        m_ernest.addInteraction(">", "f",  -10);// Bump
//        m_ernest.addInteraction("v", "f",  -3); // Right 
//        m_ernest.addInteraction("^", "f",  -3); // Left 
        
		cognitiveMode = AGENT_RUN;
        mTranslation = new Vector3f();
        mRotation  = new Vector3f();
        
        System.out.println("Ernest initialized") ;
    }
    
    /**
     * Initialize the agent's parameters
     */
    protected void initAgent()
    {
    }
    
	public void setDisplay(){
		
		boolean dispSpaceMemory=true;
		boolean dispEyeView=true;
		boolean dispInnerEar=false;
		
		int size;
		int i;
		boolean found;
		
		// Set the Spatial memory display
		if (dispSpaceMemory){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				System.out.println(m_env.frameList.get(i).getClass().getName());
				if (m_env.frameList.get(i).getClass().getName().equals("memory110.SpaceMemoryFrame")) found=true;
				i++;
			}
			if (!found) 
				{
				 	JFrame simulationFrame = new SpaceMemoryFrame(m_SpaceMemory);
					m_env.frameList.add(simulationFrame);
					//m_simulationFrame = simulationFrame;					 
				}
			else  
			{
				((SpaceMemoryFrame)m_env.frameList.get(i-1)).setMemory(m_SpaceMemory);
				//m_simulationFrame = (SpaceMemoryFrame)m_env.frameList.get(i-1);
			}
		}
		
//		// Set the Spatial memory simulation display
//		if (dispSpaceMemory){
//			size=m_env.frameList.size();
//			i=0;
//			found=false; 
//			while (i<size && !found){
//				System.out.println(m_env.frameList.get(i).getClass().getName());
//				if (m_env.frameList.get(i).getClass().getName().equals("memory110.SpaceMemorySimulationFrame")) found=true;
//				i++;
//			}
//			if (!found) 
//			{
//				JFrame simulationFrame = new SpaceMemorySimulationFrame(m_SpaceMemory);
//				//m_env.frameList.add(new SpaceMemorySimulationFrame(m_SpaceMemory));
//				m_env.frameList.add(simulationFrame);
//				//m_simulationFrame = simulationFrame;
//			}
//			else        
//			{
//				((SpaceMemorySimulationFrame)m_env.frameList.get(i-1)).setMemory(m_SpaceMemory);
//				//m_simulationFrame = (SpaceMemorySimulationFrame)m_env.frameList.get(i-1);
//				//if (getErnest() != null) getErnest().setFrame(m_env.frameList.get(i-1));
//			}
//		}
		
		// Set the vision display
		if (dispEyeView){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				if (m_env.frameList.get(i).getClass().getName().equals("agent.EyeView")) found=true;
				i++;
			}
		
			if (!found) m_env.frameList.add(new EyeView(m_eye)); 
			else        ((EyeView) m_env.frameList.get(i-1)).setEye(m_eye);
		}
		
		// Set the inner ear display
		if (dispInnerEar){
			size=m_env.frameList.size();
			i=0;
			found=false; 
			while (i<size && !found){
				if (m_env.frameList.get(i).getClass().getName().equals("InnerEar")) found=true;
				i++;
			}	
			if (!found) m_env.frameList.add(new InnerEarFrame(m_ear)); 
			else        ((InnerEarFrame) m_env.frameList.get(i-1)).setInnerEar(m_ear);
		}
	}
    
    /**
     * Update the agent when the environment is refreshed.
     * (not necessarily a cognitive step for the agent).
     */
    public void update()
    {
		//String schema = m_ernest.step(m_status);
		String schema = m_ernest.step(m_observation);
		
		int[] intention = new int[2]; 
		intention[0]= schema.toCharArray()[0];

		
        if (cognitiveMode == AGENT_STEP)
        	cognitiveMode = AGENT_STOP;

		enactSchema(intention);		
		
        anim();
    }
    
    /**
     * Enact the primitive schema chosen by Ernest.
     */
    private void enactSchema(int[] schema)
    {
    	m_observation = new Observation();
    	String status = "";
        
        // Touch
        if (m_tracer != null)
			m_tracer.addEventElement("touch_color", ErnestUtils.hexColor(focusColor.getRGB()));

    	focusColor = UNANIMATED_COLOR;
        leftColor =  UNANIMATED_COLOR;
        rightColor =  UNANIMATED_COLOR;

        int delayMove = 10;
        int delayTouch = 100;
    	m_animOrientation = 0;
    	m_animPosition = 0;

        if (schema[0] != 0)
    	{
    		
	        m_schema = Character.toString((char)schema[0]);
	        int impulsion = schema[1];
	
	        // A new interaction cycle is starting
	        System.out.println("Agent #"+ident+", Step #" + getCounter() + "=======");
	        
	        if (schema[0] == 'v')
	        {
	        	//mOrientation.z -= Math.PI/2;
	        	for (int i = 0; i < 20; i++ )
	        	{
	        		mOrientation.z -= Math.PI/40;
	        		m_animOrientation -= Math.PI/40;
	        		anim();
	        		sleep(delayMove);
	        	}
	        	if (mOrientation.z < - Math.PI) mOrientation.z += 2 * Math.PI; 
	        	status = FEEDBACK_FALSE;
	        }
	        else if (schema[0] == '^')
	        {
	        	//mOrientation.z += Math.PI/2;
	        	for (int i = 0; i < 20; i++ )
	        	{
	        		mOrientation.z += Math.PI/40;
	        		m_animOrientation += Math.PI/40;
	        		anim();
	        		sleep(delayMove);
	        	}
	        	if (mOrientation.z > Math.PI) mOrientation.z -= 2 * Math.PI; 
	        	status = FEEDBACK_FALSE;
	        }
	        else if (schema[0] == '>')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
	            if (m_env.affordWalk(point))
	            {
	            	//mPosition.set(localToParentRef(new Vector3f(DIRECTION_AHEAD)));
		        	for (int i = 0; i < 20; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(.05f,0,0)));
		            	m_animPosition += .05;
		        		anim();
		        		sleep(delayMove);
		        	}
		        	status = FEEDBACK_TRUE;
	            }
	            else
	            {
	            	focusColor = Color.RED;
		        	for (int i = 0; i < 5; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(.05f,0,0)));
		        		anim();
		        		sleep(20);
		        	}
		        	for (int i = 0; i < 5; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(-.05f,0,0)));
		        		anim();
		        		sleep(20);
		        	}
		        	status = FEEDBACK_FALSE;
	            }
	        }
	        else if (schema[0] == '<')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_BEHIND);
	        	Vector3f point = localToParentRef(localPoint);
	            if (m_env.affordWalk(point))
	            {
		        	for (int i = 0; i < 20; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(-.05f,0,0)));
		            	m_animPosition -= .05;
		        		anim();
		        		sleep(delayMove);
		        	}
		        	status = FEEDBACK_TRUE;
	            }
	            else
	            {
	            	focusColor = Color.RED;
		        	for (int i = 0; i < 5; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(-.05f,0,0)));
		        		anim();
		        		sleep(20);
		        	}
		        	for (int i = 0; i < 5; i++ )
		        	{
		            	mPosition.set(localToParentRef(new Vector3f(.05f,0,0)));
		        		anim();
		        		sleep(20);
		        	}
		        	status = FEEDBACK_FALSE;
	            }
	        }
	        else if (schema[0] == '-')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_AHEAD);
	        	Vector3f point = localToParentRef(localPoint);
            	Color blockColor = m_env.seeBlock(point.x, point.y);
            	focusColor = blockColor;//Environment.WALL1;
    	    	ErnestModel entity = m_env.getEntity(point, mName);
	            if (m_env.affordWalk(point))
	            {
	            	if (blockColor.equals(m_env.FIELD_COLOR))	
	            		status = FEEDBACK_FALSE;
	            	else
	            		status = FEEDBACK_ALGA;
	            	if (entity != null)
	            	{
	            		status = FEEDBACK_ALGA;
	                	focusColor = entity.getColor();	            		
	            	}
	            }
	            else
	            {
	            	if (blockColor.equals(m_env.WALL1))	
	            		status = FEEDBACK_TRUE;
	            	else
	            		status = FEEDBACK_BRICK;
	            }
        		anim();
        		sleep(delayTouch);
	        }
	        else if (schema[0] == '/')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_LEFT);
	        	Vector3f point = localToParentRef(localPoint);
	            if (m_env.affordWalk(point))
	            {
	            	status = FEEDBACK_FALSE;
	            	leftColor = Color.WHITE;
	            }
	            else
	            {
	            	status = FEEDBACK_TRUE;
	            	leftColor = Environment.WALL1;
	            	//Color blockColor = m_env.seeBlock(point.x, point.y);
	            	//leftColor = blockColor;//Environment.WALL1;
	            }
        		anim();
        		sleep(delayTouch);
	        }
	        else if (schema[0] == '\\')
	        {
	        	Vector3f localPoint = new Vector3f(DIRECTION_RIGHT);
	        	Vector3f point = localToParentRef(localPoint);
	            if (m_env.affordWalk(point))
	            {
	            	status = FEEDBACK_FALSE;
	            	rightColor = Color.WHITE;
	            }
	            else
	            {
	            	status = FEEDBACK_TRUE;
	            	rightColor = Environment.WALL1;
	            	//Color blockColor = m_env.seeBlock(point.x, point.y);
	            	//rightColor = blockColor;//Environment.WALL1;
	            }
        		anim();
        		sleep(delayTouch);
	        }
	        
	    	// Vision
//	        if (m_tracer != null)
//	        {
//	        	Object retina = m_tracer.addEventElement("retina");
//				m_tracer.addSubelement(retina, "pixel_0", ErnestUtils.hexColor(pixelColor[0].getRGB()));
//				m_tracer.addSubelement(retina, "pixel_1", ErnestUtils.hexColor(pixelColor[1].getRGB()));
//	        }
	        
			m_previousLeftPixel  = m_currentLeftPixel;
	        m_previousRightPixel = m_currentRightPixel;
	        eyeFixation = getRetina(mOrientation.z);
	        
	        m_observation.setVisualStimuli(0, eyeFixation[0].mRight.getRGB());
	        m_observation.setVisualStimuli(1, eyeFixation[1].mRight.getRGB());
	        m_observation.setVisualDistance(0, eyeFixation[0].mLeft);
	        m_observation.setVisualDistance(1, eyeFixation[1].mLeft);
	        for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
	            pixelColor[i] = eyeFixation[i].mRight;
//	        m_currentRightPixel  = eyeFixation[0].mLeft;
//	        m_currentLeftPixel   = eyeFixation[1].mLeft;
//	        
//	        m_satisfaction = 0;
//	        
//	        // The sensed features correspond to changes in the pixels.
//	        m_leftFeature  = sensePixel(m_previousLeftPixel, m_currentLeftPixel);
//	        m_rightFeature = sensePixel(m_previousRightPixel, m_currentRightPixel);  
//	        if (m_leftFeature.equals(" ") && m_rightFeature.equals(" "))
//	        	{m_leftFeature = ""; m_rightFeature = "";}
//	    	
//	        m_observation.setStimuli(m_leftFeature + m_rightFeature + status);
	        m_observation.setStimuli(status);
	        
	        // Trace the environmental data
	        if (m_tracer != null)
	        {
				m_tracer.addEventElement("x", mPosition.x + "");
				m_tracer.addEventElement("y", mPosition.y + "");
				m_tracer.addEventElement("orientation", mOrientation.z + "");
	        }               
    	}
    }

    private String sensePixel(int previousPixel, int currentPixel) 
    {
            String feature = " ";
            int satisfaction = 0;
            
            // arrived
            if (previousPixel > currentPixel && currentPixel == 0)
            {
                    feature = "x";
                    satisfaction = 100;
            }
            
            // closer
            else if (previousPixel < Ernest.INFINITE && currentPixel < previousPixel)
            {
                    feature = "+";
                    satisfaction = 100;
            }

            // appear
            else if (previousPixel == Ernest.INFINITE && currentPixel < Ernest.INFINITE)
            {
                    feature = "*";
                    satisfaction = 150;
            }
            
            // disappear
            else if (previousPixel < Ernest.INFINITE && currentPixel == Ernest.INFINITE)
            {
                    feature = "o";
                    satisfaction = -150;
            }

            System.out.println("Sensed " + "prev=" + previousPixel + "cur=" + currentPixel + " feature " + feature);
            
            m_satisfaction += satisfaction;

            return feature;
    }
    
    /**
     * Paint Ernest as a shark.
     * @param g The graphic object for painting.
     */
    public void paintAgent(Graphics2D g2d,int x,int y,double sx,double sy)
    {
        // The orientation

        AffineTransform orientation = new AffineTransform();
        orientation.translate(x,y);
        orientation.rotate(-mOrientation.z+Math.PI/2);
        orientation.scale(sx,sy);
        g2d.transform(orientation);

        // The shark body

        Area shark = shape(ident);
        
        // Retina pixel
        
        Arc2D.Double pixelIn = new Arc2D.Double(-20, -20, 40, 40,0, 180 / Ernest.RESOLUTION_RETINA + 1, Arc2D.PIE);
//        Arc2D.Double focus = new Arc2D.Double(-10, -35, 20, 20,0, 180, Arc2D.PIE);
//        Arc2D.Double left = new Arc2D.Double(-10, -35, 20, 20,90, 180, Arc2D.PIE);
//        Arc2D.Double right = new Arc2D.Double(-10, -35, 20, 20,-90, 180, Arc2D.PIE);
        Rectangle2D.Double focus = new Rectangle2D.Double(-12, -40, 25, 30);
        Rectangle2D.Double left = new Rectangle2D.Double(-35, -10, 25, 30);
        Rectangle2D.Double right = new Rectangle2D.Double(10, -10, 25, 30);
        
        // The tactile matrix
        
        Area[][] somatoMap = new Area[3][3];
        somatoMap[0][0] = new Area(new Rectangle2D.Double(-49, -47, 42, 40));
        somatoMap[0][0].intersect(shark);
        somatoMap[1][0] = new Area(new Rectangle2D.Double( -8, -47, 16, 40));
        somatoMap[1][0].intersect(shark);
        somatoMap[2][0] = new Area(new Rectangle2D.Double(  7, -47, 42, 40));
        somatoMap[2][0].intersect(shark);
        
        somatoMap[0][1] = new Area(new Rectangle2D.Double(-49,  -8, 42, 16));
        somatoMap[0][1].intersect(shark);
        somatoMap[1][1] = new Area(new Rectangle2D.Double( -8,  -8, 16, 16));
        somatoMap[1][1].intersect(shark);
        somatoMap[2][1] = new Area(new Rectangle2D.Double(  7,  -8, 42, 16));
        somatoMap[2][1].intersect(shark);

        somatoMap[0][2] = new Area(new Rectangle2D.Double(-49,   6, 42, 42));
        somatoMap[0][2].intersect(shark);
        somatoMap[1][2] = new Area(new Rectangle2D.Double( -8,   6, 16, 42));
        somatoMap[1][2].intersect(shark);
        somatoMap[2][2] = new Area(new Rectangle2D.Double(  7,   6, 42, 42));
        somatoMap[2][2].intersect(shark);

        // Draw the body
        
//        g2d.setColor(ErnestModel.AGENT_COLOR);
//        for (int i = 0; i < 3; i++)
//            for (int j = 0; j < 3; j++)
//            {
//                //g2d.setColor(somatoMapColor[i][j]);
//                g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_SOFT));
//            	if (m_ernest.getValue(i, j) == Ernest.PHENOMENON_EMPTY)
//            		g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_EMPTY));
//            	if (m_ernest.getValue(i, j) == Ernest.PHENOMENON_WALL)
//            		g2d.setColor(new Color(Ernest.STIMULATION_TOUCH_WALL));
//                g2d.fill(somatoMap[i][j]);
//            }
        
		AffineTransform ref0 = g2d.getTransform();
        AffineTransform r = new AffineTransform();
        r.rotate(-Math.PI/2);
        r.scale(.8,.8);
        g2d.transform(r);

        Polygon agent = new Polygon();agent.addPoint(-50,-40);agent.addPoint(-30,0);agent.addPoint(-50,40);agent.addPoint(50,0);
		g2d.setColor(new Color(0xFF8000));
        g2d.fill(agent);
		g2d.setStroke(new BasicStroke(4f));
		g2d.setColor(Color.black);
        g2d.draw(agent);
        g2d.setTransform(ref0);

        
        // Draw the focus
        
		g2d.setStroke(new BasicStroke(2f));
    	g2d.setColor(leftColor);
        if (leftColor != UNANIMATED_COLOR)
        {
        	g2d.fill(left);
        	g2d.setColor(Color.black);
        	g2d.draw(left);
        }
        g2d.setColor(rightColor);
        if (rightColor != UNANIMATED_COLOR)
        {
        	g2d.fill(right);
        	g2d.setColor(Color.black);
        	g2d.draw(right);
        }
        if (focusColor != UNANIMATED_COLOR)
        {
        	g2d.setColor(focusColor);
        	g2d.fill(focus);
        	g2d.setColor(Color.black);
        	g2d.draw(focus);
        }
        
        // Draw the retina
        
        AffineTransform transformColliculus = new AffineTransform();
        transformColliculus.rotate(0);
        transformColliculus.translate(0,0);
        g2d.transform(transformColliculus);
        AffineTransform RetinaReference = g2d.getTransform();
        AffineTransform transformSegment = new AffineTransform();
        g2d.transform(transformSegment);
        transformSegment.rotate( - Math.PI / Ernest.RESOLUTION_RETINA);
        g2d.setColor(Color.BLACK);
        
        g2d.setTransform(RetinaReference);
        for (int i = 0; i < Ernest.RESOLUTION_RETINA; i++)
        {
            g2d.setColor(pixelColor[i]);
            g2d.fill(pixelIn);
            g2d.transform(transformSegment);
        }

    }

    /**
     * The shape is centered in (0,0) and fits in a 100x100 rectangle.
     * The pelvic fin pattern represents the agent's id number in binary code.
     * @param ID The agent's id number.
     * @return The shark area.
     */
    public static Area shape(int ID)
    {
        GeneralPath body = new GeneralPath();
        body.append(new CubicCurve2D.Double(0,-40, -30,-40, -5, 45, 0, 45), false);
        body.append(new CubicCurve2D.Double(0, 45,   5, 45, 30,-40, 0,-40), false);
        
        GeneralPath leftPectoralFin = new GeneralPath();
        leftPectoralFin.append(new CubicCurve2D.Double(-15, -15, -30, -10, -40, 0, -40, 20), false);
        leftPectoralFin.append(new CubicCurve2D.Double(-40,  20, -30,  10, -20, 8, -10, 10), true);

        GeneralPath leftPelvicFin = new GeneralPath();
        leftPelvicFin.append(new CubicCurve2D.Double(-10, 15, -15, 18, -20, 25, -15, 30), false);
        leftPelvicFin.append(new CubicCurve2D.Double(-15, 30,  -10, 25, -10,  25,  -5, 28), true);

        GeneralPath rightPectoralFin = new GeneralPath();
        rightPectoralFin.append(new CubicCurve2D.Double(15, -15, 30, -10, 40, 0, 40, 20), false);
        rightPectoralFin.append(new CubicCurve2D.Double(40,  20, 30,  10, 20, 8, 10, 10), true);

        GeneralPath rightPelvicFin = new GeneralPath();
        rightPelvicFin.append(new CubicCurve2D.Double(10, 15, 15, 18, 20, 25, 15, 30), false);
        rightPelvicFin.append(new CubicCurve2D.Double(15, 30, 10, 25, 10, 25,  5, 28), true);

        GeneralPath caudalFin = new GeneralPath();
        caudalFin.append(new CubicCurve2D.Double(10, 50, 15, 20, -15, 20, -10, 50), false);
        caudalFin.append(new CubicCurve2D.Double(-10, 50, -15, 30, 15, 30, 10, 50), false);

        Area shark = new Area(body);
    	shark.add(new Area(leftPectoralFin));
        if ((ID & 1) == 0)
        	shark.add(new Area(leftPelvicFin));
        shark.add(new Area(rightPectoralFin));
        if ((ID & 2) == 0)
        	shark.add(new Area(rightPelvicFin));
        
        return shark;
    }    
    
    private void anim()
    {
        // compute absolute movements
		mSpeedT=new Vector3f(mPosition);
		mSpeedT.sub(mPreviousPosition);
		
		mSpeedR=new Vector3f(mOrientation);
		mSpeedR.sub(mPreviousOrientation);
		
		Matrix3f rot2 = new Matrix3f();
		rot2.rotZ( -mOrientation.z);
		rot2.transform(mSpeedT, mEgoSpeedT);
		
		//m_ear.computeEars(mEgoSpeedT, mSpeedR);
		
		if (mSpeedR.z > Math.PI) mSpeedR.z-=2*Math.PI;
		if (mSpeedR.z<=-Math.PI) mSpeedR.z+=2*Math.PI;
		
        //if (cognitiveMode!=AGENT_STOP) rendu();
        
        mainFrame.drawGrid();
        
        for (int i=0;i<m_env.frameList.size();i++){
			m_env.frameList.get(i).repaint();
		}
    }
    
	public void paintSpaceMemory(Graphics g, ArrayList<IPlace> placeList)
	{
		//ArrayList<IPlace> placeList = new ArrayList<IPlace>();

		//placeList = getErnest().getPlaceList();
		
		final int WIDTH = 300;
		final int HEIGHT = 250;
		final int SCALE = 50;//40; 
		Color agentColor = new Color(0xFF8000);

		boolean displayPhenomenon = true;
		
		Graphics2D g2d = (Graphics2D)g;
		
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING,RenderingHints.VALUE_ANTIALIAS_ON);

		// Display background
		//g2d.setColor(new Color(200, 200, 200));
		g2d.setColor(Color.white);
		g2d.fillRect(0, 0, 2 * WIDTH , 2 * HEIGHT);
		
        // Display counter
		String counter = getCounter() + ""; 
		Font font = new Font("Dialog", Font.BOLD, 18);
		g2d.setFont(font);
		//FontMetrics fm = getFontMetrics(font);
		//int width = fm.stringWidth(counter);
		g2d.setColor(Color.GRAY);		
		//g2d.drawString(counter, 2 * WIDTH - 30 - width, 30);	
		g2d.drawString(counter, 2 * WIDTH - 50, 30);	
		
		float agentOrientation = 0;
//		for (IPlace place : spaceMemory.getPlaceList())
//		{
//			if (place.getType() == Spas.PLACE_FOCUS)
//			{
//				refAngle = place.getDirection();
//			}
//		}

		//IPlace focusPlace = getErnest().getFocusPlace();
		agentOrientation = getOrientation();
		
		//float baseOrientation = - agentOrientation + focusPlace.getOrientation();
		float baseOrientation = agentOrientation; // allocentric
		//float baseOrientation = - focusPlace.getOrientation(); // target centric.
		//float baseOrientation = 0; // agent horizontal
		//float x = (float)Math.cos(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		//float y = (float)Math.sin(baseOrientation + focusPlace.getDirection()) * focusPlace.getDistance();
		
		AffineTransform ref0 = g2d.getTransform();
		AffineTransform ref1 = new AffineTransform();
		//ref1.translate( - x * SCALE,  y * SCALE);
        //ref1.rotate(- baseOrientation, WIDTH, HEIGHT);
		ref1.translate( - m_animPosition * SCALE,  0);
        ref1.rotate(m_animOrientation, WIDTH, HEIGHT);

        g2d.transform(ref1);
        //g2d.setTransform(ref0);
		
		// Reference transformation
        AffineTransform ref = g2d.getTransform();
        
        //g2d.setTransform(ref);
        
		double d;
		double rad;
		double angle;
		double span;
		
        // Display the places
		//g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER));
		g2d.setStroke(new BasicStroke(SCALE / 3f, BasicStroke.CAP_ROUND, BasicStroke.JOIN_MITER));

		Ellipse2D.Double circle = new Ellipse2D.Double(-10, -10, 20, 20); 
        Arc2D.Double pie = new Arc2D.Double(-10, -10, 20, 20,-90, 180, Arc2D.PIE);
        Polygon triangle = new Polygon();triangle.addPoint(-10,-10);triangle.addPoint(-10,10);triangle.addPoint(10,0);
        int squareSize = 7;
        Polygon square = new Polygon();square.addPoint(-squareSize,-squareSize);square.addPoint(-squareSize,squareSize);square.addPoint(squareSize,squareSize);square.addPoint(squareSize,-squareSize);
        //Polygon tile = new Polygon();tile.addPoint(-SCALE/2,-SCALE/2);tile.addPoint(-SCALE/2,SCALE/2);tile.addPoint(SCALE/2,SCALE/2);tile.addPoint(SCALE/2,-SCALE/2);
		Ellipse2D.Double tile = new Ellipse2D.Double(-SCALE * .4, -SCALE * .4, SCALE * .8, SCALE * .8); 
        Polygon agent = new Polygon();agent.addPoint(-50,-50);agent.addPoint(-30,0);agent.addPoint(-50,50);agent.addPoint(50,0);

 		// Display the phenomenon
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		AffineTransform or;
		//for (IPlace place : getErnest().getPlaceList())
		for (IPlace place : placeList)
		{
			if (place.getType() == Spas.PLACE_PHENOMENON || place.getType() == Spas.PLACE_COPRESENCE)
			{
				//g2d.setColor(new Color(place.getValue()));
				int scale = (int)(127 + 128 * (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION);
				if (scale > 255) scale = 255;
				g2d.setColor(new Color(scale, scale, scale));		
				
		        ref = g2d.getTransform();
		        or = new AffineTransform();
		        or.translate(WIDTH + (int)(place.getPosition().x * SCALE), HEIGHT - (int)(place.getPosition().y * SCALE));
		        //or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
		        or.rotate(- place.getOrientation());
		        g2d.transform(or);
				//g2d.fill(tile);
				//g2d.setColor(Color.gray);
				//if (place.getBundle().getValue() == Ernest.PHENOMENON_WALL)
				//	g2d.setColor(new Color(place.getValue()));
				g2d.draw(tile);
		        g2d.setTransform(ref);
		        
		        for (IAct a : place.getBundle().getActList())
		        {
					g2d.setColor(new Color(a.getColor()));		
					Shape shape = circle;
					int offsetx = 0;
					int offsety = 0;
					float orientation = 0;
					if (a.getLabel().indexOf(">") >=0)
					{
						shape = triangle;
						//if (a.getLabel().equals(">f"))
							//g2d.setColor(new Color(255, 80, 80));
					}
					if (a.getLabel().equals("^f"))
					{
						shape = pie;
						orientation = (float) - Math.PI / 2;
						offsety = SCALE/ 4;
					}
					if (a.getLabel().equals("vf"))
					{
						shape = pie;
						orientation = (float) Math.PI / 2;
						offsety = - SCALE/ 4;
					}
					if (a.getLabel().equals("/f") || a.getLabel().equals("/t"))
					{
						shape = square;
						offsetx = - SCALE /4;
						offsety = - SCALE/ 3;
					}
					if (a.getLabel().equals("\\f") || a.getLabel().equals("\\t"))
					{
						shape = square;
						offsetx = - SCALE /4;
						offsety = SCALE/3;
					}
					if (a.getLabel().equals("-f") || a.getLabel().equals("-t"))
					{
						shape = square;
						offsetx = - SCALE /3;
					}
					if (a.getLabel().equals("-b"))
					{
						shape = square;
						offsetx = - SCALE /3;
						offsety = SCALE/6;
					}
					
					if (shape != circle)
					{
						ref = g2d.getTransform();
				        or = new AffineTransform();
				        float ooffx = offsetx * (float)Math.cos(- place.getOrientation()) + offsety * (float)Math.sin(- place.getOrientation());
				        float ooffy = - offsetx * (float)Math.sin(- place.getOrientation()) + offsety * (float)Math.cos(- place.getOrientation());
				        or.translate(WIDTH + (int)(place.getPosition().x * SCALE + ooffx), HEIGHT - (int)(place.getPosition().y * SCALE + ooffy));
				        //or.translate(WIDTH + (int)(place.getPosition().x * SCALE + offsetx), HEIGHT - (int)(place.getPosition().y * SCALE + offsety));
				        or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
				        or.rotate(- place.getOrientation());
				        or.rotate(orientation);
				        g2d.transform(or);
						//g2d.fill(shape);
						g2d.setColor(Color.gray);
						//g2d.draw(shape);
				        g2d.setTransform(ref);
					}
		        }
			}
		}
				
		// Display agent
		g2d.setStroke(new BasicStroke(SCALE / 10f));
        g2d.setTransform(ref);
        AffineTransform placeAgent = new AffineTransform();
        placeAgent.translate(WIDTH + m_animPosition * SCALE, HEIGHT);
        //placeAgent.rotate(Math.PI/2);
        placeAgent.scale(SCALE / 100f, SCALE / 100f);
        placeAgent.rotate(- m_animOrientation);
        g2d.transform(placeAgent);
		g2d.setColor(agentColor);
        //g2d.fill(shape(getID()));
        g2d.fill(agent);
		g2d.setColor(Color.black);
        g2d.draw(agent);
        g2d.setTransform(ref);

 		// Display the interactions
		g2d.setStroke(new BasicStroke(SCALE / 20f));
		//AffineTransform or;
		//for (IPlace place : getErnest().getPlaceList())
		for (IPlace place : placeList)
		{
			if (place.getType() == Spas.PLACE_EVOKE_PHENOMENON || place.getType() == Spas.PLACE_SIMULATION  || place.getType() == Spas.PLACE_UNKNOWN)
			{
				g2d.setColor(new Color(place.getValue()));		
								
				Shape shape = circle;
				float orientation = 0;
				int offsetx = 0;
				int offsety = 0;
				float scale = 1;
				IAct a = place.getAct();
				if (a != null)
				{
					if (a.getLabel().equals(">t"))
					{
						shape = triangle;
					}
					if (a.getLabel().equals(">f"))
					{
						shape = triangle;
						//g2d.setColor(Color.red);
					}
					if (a.getLabel().equals(">++t"))
					{
						shape = triangle;
						//g2d.setColor(Color.red);
					}
					if (a.getLabel().equals("> +t"))
					{
						shape = triangle;
						//g2d.setColor(Color.red);
					}
					if (a.getLabel().equals(">+ t"))
					{
						shape = triangle;
						//g2d.setColor(Color.red);
					}
					if (a.getLabel().equals("^f"))
					{
						shape = pie;
						orientation = 0;//(float) - Math.PI;// / 2;
						offsetx = SCALE/ 4;
					}
					if (a.getLabel().equals("^* f"))
					{
						shape = pie;
						orientation = 0;//(float) - Math.PI;// / 2;
						offsetx = SCALE/ 4;
					}
					if (a.getLabel().equals("vf"))
					{
						shape = pie;
						orientation = 0; //(float) Math.PI / 2;
						offsetx = SCALE/ 4;
					}
					if (a.getLabel().equals("v *f"))
					{
						shape = pie;
						orientation = 0; //(float) Math.PI / 2;
						offsetx = SCALE/ 4;
					}
					if (a.getLabel().indexOf("/") >=0)
					{
						shape = square;
						offsetx = - SCALE /4;
						offsety = - SCALE/ 3;
					}
					if (a.getLabel().indexOf("\\") >=0)
					{
						shape = square;
						offsetx = - SCALE /4;
						offsety = SCALE/3;
					}
					if (a.getLabel().equals("-f") || a.getLabel().equals("-t") )
					{
						shape = square;
						offsetx = - SCALE /3;
					}
					if (a.getLabel().equals("-b"))
					{
						shape = square;
						offsetx = - SCALE /3;
						g2d.setColor(Environment.WALL3);		
					}
					if (a.getLabel().equals("-a"))
					{
						shape = square;
						offsetx = - SCALE /3;
						//g2d.setColor(Environment.ALGA1);		
					}
					if (a.getLabel().equals("(^f>t)") || a.getLabel().equals("(vf>t)") )
					{
						shape = triangle;
						orientation = 0;
						offsetx = 0;
						offsety = 0;
						g2d.setColor(Color.yellow);
					}
					if (a.getLabel().equals("(-f>t)"))
					{
						shape = triangle;
						orientation = 0;
						offsetx = 0;
						offsety = 0;
						g2d.setColor(Color.pink);
					}
				}
				if (shape != circle)
				{
					ref = g2d.getTransform();
			        or = new AffineTransform();
			        float ooffx = offsetx * (float)Math.cos(- place.getOrientation()) + offsety * (float)Math.sin(- place.getOrientation());
			        float ooffy = - offsetx * (float)Math.sin(- place.getOrientation()) + offsety * (float)Math.cos(- place.getOrientation());
			        or.translate(WIDTH + (int)(place.getPosition().x * SCALE + ooffx), HEIGHT - (int)(place.getPosition().y * SCALE + ooffy));
			        //or.translate(WIDTH + (int)(place.getPosition().x * SCALE + offsetx), HEIGHT - (int)(place.getPosition().y * SCALE + offsety ));
			        //or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
			        or.rotate(- place.getOrientation());
			        or.rotate(orientation);
			        or.scale(scale, scale);
			        g2d.transform(or);
					g2d.fill(shape);
					if (place.getType() == Spas.PLACE_SIMULATION)
						g2d.setColor(agentColor);
					else if (place.getType() == Spas.PLACE_UNKNOWN)
						g2d.setColor(new Color(0x8080FF));
					else
						g2d.setColor(Color.black);
					g2d.draw(shape);
			        g2d.setTransform(ref);
					//g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
				}	
			}
		}
//		for (IPlace place : placeList)
//		{
//			if (place.getType() == Spas.PLACE_SIMULATION && place.getAct() == null)
//			{
//				g2d.setColor(Color.red);		
//				
//				Shape shape = circle;
//				float orientation = 0;
//				int offsetx = 0;
//				int offsety = 0;
//
//				ref = g2d.getTransform();
//		        or = new AffineTransform();
//		        float ooffx = offsetx * (float)Math.cos(- place.getOrientation()) + offsety * (float)Math.sin(- place.getOrientation());
//		        float ooffy = - offsetx * (float)Math.sin(- place.getOrientation()) + offsety * (float)Math.cos(- place.getOrientation());
//		        or.translate(WIDTH + (int)(place.getPosition().x * SCALE + ooffx), HEIGHT - (int)(place.getPosition().y * SCALE + ooffy));
//		        //or.translate(WIDTH + (int)(place.getPosition().x * SCALE + offsetx), HEIGHT - (int)(place.getPosition().y * SCALE + offsety ));
//		        //or.scale(( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION),( 1  - (getUpdateCount() - place.getUpdateCount())/(float)LocalSpaceMemory.PERSISTENCE_DURATION));
//		        or.rotate(- place.getOrientation());
//		        or.rotate(orientation);
//		        g2d.transform(or);
//				g2d.fill(shape);
//				g2d.setColor(Color.black);
//				g2d.draw(shape);
//		        g2d.setTransform(ref);
//				//g2d.setStroke(new BasicStroke(Math.max(SCALE / 3f * ( 1  - (spaceMemory.getUpdateCount() - place.getUpdateCount())/15f), 1), BasicStroke.CAP_ROUND, BasicStroke.JOIN_ROUND));
//			}
//		}
	}
}
