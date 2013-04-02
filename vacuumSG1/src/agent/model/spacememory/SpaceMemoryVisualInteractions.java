package agent.model.spacememory ;

import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.util.HashMap ;
import java.util.Map ;

import agent.model.VisualStimuli ;
import ernest.Ernest ;

public enum SpaceMemoryVisualInteractions {
	DEFAULT( "" ) ,
	APPEAR_APPEAR( VisualStimuli.APPEAR.getLabel() + VisualStimuli.APPEAR.getLabel() ) ,
	APPEAR_CLOSER( VisualStimuli.APPEAR.getLabel() + VisualStimuli.CLOSER.getLabel() ) ,
	APPEAR_DISAPPEAR(
			VisualStimuli.APPEAR.getLabel() + VisualStimuli.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	APPEAR_UNCHANGED( VisualStimuli.APPEAR.getLabel() + VisualStimuli.UNCHANGED.getLabel() ) ,
	CLOSER_APPEAR( VisualStimuli.CLOSER.getLabel() + VisualStimuli.APPEAR.getLabel() ) ,
	CLOSER_CLOSER( VisualStimuli.CLOSER.getLabel() + VisualStimuli.CLOSER.getLabel() ) ,
	CLOSER_DISAPPEAR(
			VisualStimuli.CLOSER.getLabel() + VisualStimuli.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	CLOSER_UNCHANGED( VisualStimuli.CLOSER.getLabel() + VisualStimuli.UNCHANGED.getLabel() ) ,
	DISAPPEAR_APPEAR(
			VisualStimuli.DISAPPEAR.getLabel() + VisualStimuli.APPEAR.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	DISAPPEAR_CLOSER(
			VisualStimuli.DISAPPEAR.getLabel() + VisualStimuli.CLOSER.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	DISAPPEAR_DISAPPEAR(
			VisualStimuli.DISAPPEAR.getLabel() + VisualStimuli.DISAPPEAR.getLabel() ,
			Ernest.INFINITE ,
			Ernest.INFINITE ) ,
	DISAPPEAR_UNCHANGED(
			VisualStimuli.DISAPPEAR.getLabel() + VisualStimuli.UNCHANGED.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	UNCHANGED_APPEAR( VisualStimuli.UNCHANGED.getLabel() + VisualStimuli.APPEAR.getLabel() ) ,
	UNCHANGED_CLOSER( VisualStimuli.UNCHANGED.getLabel() + VisualStimuli.CLOSER.getLabel() ) ,
	UNCHANGED_DISAPPEAR(
			VisualStimuli.UNCHANGED.getLabel() + VisualStimuli.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	UNCHANGED_UNCHANGED( VisualStimuli.UNCHANGED.getLabel() + VisualStimuli.UNCHANGED.getLabel() ) ;

	private final String interaction ;
	private Area leftEyeShape ;
	private Area rightEyeShape ;
	private int leftEyeDistance ;
	private int rightEyeDistance ;

	private final static Map<String , SpaceMemoryVisualInteractions> BY_VISUAL_INTERACTION = new HashMap<String , SpaceMemoryVisualInteractions>() ;

	static {
		for ( SpaceMemoryVisualInteractions smInteractions : SpaceMemoryVisualInteractions.values() ) {
			BY_VISUAL_INTERACTION.put( smInteractions.interaction , smInteractions ) ;
		}
	}

	private SpaceMemoryVisualInteractions( String interaction ) {
		this.interaction = interaction ;
		this.changeLeftEyeDistance( 0 ) ;
		this.changeRightEyeDistance( 0 ) ;
	}

	private SpaceMemoryVisualInteractions( String interaction ,
			int leftEyeDistance ,
			int rightEyeDistance ) {
		this.interaction = interaction ;
		this.changeLeftEyeDistance( leftEyeDistance ) ;
		this.changeRightEyeDistance( rightEyeDistance ) ;
	}

	private void changeLeftEyeDistance( int distance ) {
		this.leftEyeDistance = distance ;
		if ( distance == Ernest.INFINITE ) {
			this.leftEyeShape = circleShape( 0 ) ;
		} else {
			this.leftEyeShape = circleShape( distance ) ;
		}

	}

	private void changeRightEyeDistance( int distance ) {
		this.rightEyeDistance = distance ;
		if ( distance == Ernest.INFINITE ) {
			this.rightEyeShape = circleShape( 0 ) ;
		} else {
			this.rightEyeShape = circleShape( distance ) ;
		}
	}

	private static Area circleShape( int diameter ) {
		GeneralPath shape = new GeneralPath() ;
		shape.append( new Ellipse2D.Double(
				0 - ( diameter / 2 ) ,
				0 - ( diameter / 2 ) ,
				diameter ,
				diameter ) , true ) ;
		return new Area( shape ) ;
	}

	public static SpaceMemoryVisualInteractions getSpaceMemoryVisualInteraction(
			String visualInteraction , int leftEyeDistance , int rightEyeDistance ) {
		if ( BY_VISUAL_INTERACTION.containsKey( visualInteraction ) ) {
			SpaceMemoryVisualInteractions smVisualInteraction = BY_VISUAL_INTERACTION
					.get( visualInteraction ) ;
			smVisualInteraction.changeLeftEyeDistance( leftEyeDistance ) ;
			smVisualInteraction.changeRightEyeDistance( rightEyeDistance ) ;
			return smVisualInteraction ;
		} else {
			return SpaceMemoryVisualInteractions.DEFAULT ;
		}
	}

	public static boolean containVisualInteraction( String interaction ) {
		return interaction.contains( VisualStimuli.APPEAR.getLabel() ) ||
				interaction.contains( VisualStimuli.DISAPPEAR.getLabel() ) ||
				interaction.contains( VisualStimuli.CLOSER.getLabel() ) ||
				interaction.contains( VisualStimuli.UNCHANGED.getLabel() ) ;
	}

	public static String extractVisualInteraction( String interaction ) {
		String visualInteraction = "" ;
		for ( char inter : interaction.toCharArray() ) {
			if ( VisualStimuli.isExist( String.valueOf( inter ) ) ) {
				visualInteraction += inter ;
			}
		}

		if ( visualInteraction.length() != 2 )
			throw new RuntimeException( "Can't extract the visual interaction" ) ;
		return visualInteraction ;
	}

	public String getInteraction() {
		return this.interaction ;
	}

	public int getLeftEyeDistance() {
		return this.leftEyeDistance ;
	}

	public Area getLeftEyeShape() {
		return this.leftEyeShape ;
	}

	public int getRightEyeDistance() {
		return this.rightEyeDistance ;
	}

	public Area getRightEyeShape() {
		return this.rightEyeShape ;
	}
}
