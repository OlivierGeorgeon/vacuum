package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.TactileEffect ;

import java.awt.Color ;
import java.util.HashMap ;
import java.util.Map ;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public enum SpaceMemoryTactileEffect {
	DEFAULT( "" , Color.WHITE ) ,
	FALSE( TactileEffect.FALSE.getLabel() , Color.RED ) ,
	TRUE( TactileEffect.TRUE.getLabel() , Color.WHITE ) ,
	BRICK( TactileEffect.BRICK.getLabel() , Color.WHITE ) ,
	ALGA( TactileEffect.ALGA.getLabel() , Color.WHITE ) ,
	FOOD( TactileEffect.FOOD.getLabel() , Color.WHITE ) ;
	
	private final String tactileEffectLabel ;
	private Color effectColor;

	private final static Map<String , SpaceMemoryTactileEffect> BY_TACTILE_EFFECT_LABEL = new HashMap<String , SpaceMemoryTactileEffect>() ;

	static {
		for ( SpaceMemoryTactileEffect smInteractions : SpaceMemoryTactileEffect.values() ) {
			BY_TACTILE_EFFECT_LABEL.put( smInteractions.tactileEffectLabel , smInteractions ) ;
		}
	}

	private SpaceMemoryTactileEffect( String tactileEffectLabel , Color effectColor ) {
		this.tactileEffectLabel = tactileEffectLabel ;
		this.effectColor = effectColor;
	}

	public static SpaceMemoryTactileEffect getSpaceMemoryTactileEffect(String tactileEffectLabel ) {
		if ( BY_TACTILE_EFFECT_LABEL.containsKey( tactileEffectLabel ) ) {
			return BY_TACTILE_EFFECT_LABEL.get( tactileEffectLabel ) ;
		} else {
			return SpaceMemoryTactileEffect.DEFAULT ;
		}
	}

	public static String extractTactileEffectLabel( String interaction ) {
		for ( char inter : interaction.toCharArray() ) {
			if ( TactileEffect.isExist( String.valueOf( inter ) ) ) {
				return String.valueOf( inter ) ;
			}
		}
		throw new RuntimeException( "Can't extract the tactile effect" ) ;
	}
		
	public Color getEffectColor() {
		return this.effectColor ;
	}
}
