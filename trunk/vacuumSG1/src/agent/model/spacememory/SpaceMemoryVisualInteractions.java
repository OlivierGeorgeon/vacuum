package agent.model.spacememory ;

import java.awt.geom.Area ;
import java.awt.geom.Ellipse2D ;
import java.awt.geom.GeneralPath ;
import java.util.HashMap ;
import java.util.Map ;

import agent.model.VisualEffect ;
import ernest.Ernest ;

public enum SpaceMemoryVisualInteractions {
	DEFAULT( "" ) ,
	APPEAR_APPEAR( VisualEffect.APPEAR.getLabel() + VisualEffect.APPEAR.getLabel() ) ,
	APPEAR_CLOSER( VisualEffect.APPEAR.getLabel() + VisualEffect.CLOSER.getLabel() ) ,
	APPEAR_DISAPPEAR(
			VisualEffect.APPEAR.getLabel() + VisualEffect.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	APPEAR_UNCHANGED( VisualEffect.APPEAR.getLabel() + VisualEffect.UNCHANGED.getLabel() ) ,
	CLOSER_APPEAR( VisualEffect.CLOSER.getLabel() + VisualEffect.APPEAR.getLabel() ) ,
	CLOSER_CLOSER( VisualEffect.CLOSER.getLabel() + VisualEffect.CLOSER.getLabel() ) ,
	CLOSER_DISAPPEAR(
			VisualEffect.CLOSER.getLabel() + VisualEffect.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	CLOSER_UNCHANGED( VisualEffect.CLOSER.getLabel() + VisualEffect.UNCHANGED.getLabel() ) ,
	DISAPPEAR_APPEAR(
			VisualEffect.DISAPPEAR.getLabel() + VisualEffect.APPEAR.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	DISAPPEAR_CLOSER(
			VisualEffect.DISAPPEAR.getLabel() + VisualEffect.CLOSER.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	DISAPPEAR_DISAPPEAR(
			VisualEffect.DISAPPEAR.getLabel() + VisualEffect.DISAPPEAR.getLabel() ,
			Ernest.INFINITE ,
			Ernest.INFINITE ) ,
	DISAPPEAR_UNCHANGED(
			VisualEffect.DISAPPEAR.getLabel() + VisualEffect.UNCHANGED.getLabel() ,
			Ernest.INFINITE ,
			0 ) ,
	UNCHANGED_APPEAR( VisualEffect.UNCHANGED.getLabel() + VisualEffect.APPEAR.getLabel() ) ,
	UNCHANGED_CLOSER( VisualEffect.UNCHANGED.getLabel() + VisualEffect.CLOSER.getLabel() ) ,
	UNCHANGED_DISAPPEAR(
			VisualEffect.UNCHANGED.getLabel() + VisualEffect.DISAPPEAR.getLabel() ,
			0 ,
			Ernest.INFINITE ) ,
	UNCHANGED_UNCHANGED( VisualEffect.UNCHANGED.getLabel() + VisualEffect.UNCHANGED.getLabel() ) ;

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
		return interaction.contains( VisualEffect.APPEAR.getLabel() ) ||
				interaction.contains( VisualEffect.DISAPPEAR.getLabel() ) ||
				interaction.contains( VisualEffect.CLOSER.getLabel() ) ||
				interaction.contains( VisualEffect.UNCHANGED.getLabel() ) ;
	}

	public static String extractVisualInteraction( String interaction ) {
		String visualInteraction = "" ;
		for ( char inter : interaction.toCharArray() ) {
			if ( VisualEffect.isExist( String.valueOf( inter ) ) ) {
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
