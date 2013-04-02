package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum VisualStimuli {
	APPEAR( "*" ) ,
	CLOSER( "+" ) ,
	DISAPPEAR( "o" ) ,
	UNCHANGED( " " ) ;

	private final static Map<String , VisualStimuli> BY_LABEL = new HashMap<String , VisualStimuli>() ;
	private final String label ;

	static {
		for ( VisualStimuli stimuli : VisualStimuli.values() ) {
			BY_LABEL.put( stimuli.label , stimuli ) ;
		}
	}

	private VisualStimuli( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}

	public static boolean isExist( String label ) {
		return BY_LABEL.containsKey( label ) ;
	}

	public static VisualStimuli getByLabel( String label ) throws IllegalArgumentException {
		if ( BY_LABEL.containsKey( label ) ) {
			return BY_LABEL.get( label ) ;
		}

		throw new IllegalArgumentException() ;
	}
}
