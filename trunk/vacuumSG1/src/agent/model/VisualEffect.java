package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum VisualEffect {
	APPEAR( "*" ) ,
	CLOSER( "+" ) ,
	DISAPPEAR( "o" ) ,
	UNCHANGED( " " ) ;

	private final static Map<String , VisualEffect> BY_LABEL = new HashMap<String , VisualEffect>() ;
	private final String label ;

	static {
		for ( VisualEffect stimuli : VisualEffect.values() ) {
			BY_LABEL.put( stimuli.label , stimuli ) ;
		}
	}

	private VisualEffect( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}

	public static boolean isExist( String label ) {
		return BY_LABEL.containsKey( label ) ;
	}

	public static VisualEffect getByLabel( String label ) throws IllegalArgumentException {
		if ( BY_LABEL.containsKey( label ) ) {
			return BY_LABEL.get( label ) ;
		}

		throw new IllegalArgumentException() ;
	}
}
