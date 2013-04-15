package ideal.vacuum.agent.spacememory ;

import ideal.vacuum.agent.VisualEffect ;

import java.awt.Color ;
import java.util.HashMap ;
import java.util.Map ;


/**
 * 
 * @author Joseph GARNIER
 * @version $Revision$
 */
public enum SpaceMemoryVisualEffect {
	DEFAULT( "" , Color.WHITE ) ,
	APPEAR( VisualEffect.APPEAR.getLabel() , new Color(0x80E000) ) ,
	CLOSER( VisualEffect.CLOSER.getLabel() , new Color(0x00c000) ) ,
	DISAPPEAR( VisualEffect.DISAPPEAR.getLabel() , new Color(0x808080) ) ,
	UNCHANGED( VisualEffect.UNCHANGED.getLabel() , new Color(0xc0c0c0) ) ;
	
	private final String visualEffectLabel ;
	private Color effectColor;

	private final static Map<String , SpaceMemoryVisualEffect> BY_VISUAL_EFFECT_LABEL = new HashMap<String , SpaceMemoryVisualEffect>() ;

	static {
		for ( SpaceMemoryVisualEffect smInteractions : SpaceMemoryVisualEffect.values() ) {
			BY_VISUAL_EFFECT_LABEL.put( smInteractions.visualEffectLabel , smInteractions ) ;
		}
	}

	private SpaceMemoryVisualEffect( String visualEffectLabel , Color effectColor ) {
		this.visualEffectLabel = visualEffectLabel ;
		this.effectColor = effectColor;
	}

	public static SpaceMemoryVisualEffect getSpaceMemoryVisualEffect(String visualEffectLabel ) {
		if ( BY_VISUAL_EFFECT_LABEL.containsKey( visualEffectLabel ) ) {
			return BY_VISUAL_EFFECT_LABEL.get( visualEffectLabel ) ;
		} else {
			return SpaceMemoryVisualEffect.DEFAULT ;
		}
	}

	public static boolean containVisualEffect( String interaction ) {
		return interaction.contains( VisualEffect.APPEAR.getLabel() ) ||
				interaction.contains( VisualEffect.DISAPPEAR.getLabel() ) ||
				interaction.contains( VisualEffect.CLOSER.getLabel() ) ||
				interaction.contains( VisualEffect.UNCHANGED.getLabel() ) ;
	}

	public static String extractLeftVisualEffectLabel( String interaction ) {
		for ( char inter : interaction.toCharArray() ) {
			if ( VisualEffect.isExist( String.valueOf( inter ) ) ) {
				return String.valueOf( inter ) ;
			}
		}
		throw new RuntimeException( "Can't extract the visual effect" ) ;
	}
	
	public static String extractRightVisualEffectLabel( String interaction ) {
		int eye = 0;
		for ( char inter : interaction.toCharArray() ) {
			if ( VisualEffect.isExist( String.valueOf( inter ) ) ) {
				eye++;
			}
			if ( eye == 2 ) {
				return String.valueOf( inter ) ;
			}
		}
		throw new RuntimeException( "Can't extract the visual effect" ) ;
	}
	
	public Color getEffectColor() {
		return this.effectColor ;
	}
}
