package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum TactileStimuli {
	FALSE( "f" ) ,
	TRUE( "t" ) ,
	BRICK( "b" ) ,
	ALGA( "a" ) ,
	FOOD( "a" ) ;

	private final static Map<String , TactileStimuli> BY_LABEL = new HashMap<String , TactileStimuli>() ;
	private final String label ;

	static {
		for ( TactileStimuli stimuli : TactileStimuli.values() ) {
			BY_LABEL.put( stimuli.label , stimuli ) ;
		}
	}

	private TactileStimuli( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}

	public static boolean isExist( String label ) {
		return BY_LABEL.containsKey( label ) ;
	}

	public static TactileStimuli getByLabel( String label ) throws IllegalArgumentException {
		if ( BY_LABEL.containsKey( label ) ) {
			return BY_LABEL.get( label ) ;
		}

		throw new IllegalArgumentException() ;
	}
}
