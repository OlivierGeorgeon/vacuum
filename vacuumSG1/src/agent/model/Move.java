package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum Move {
	MOVE_FORWARD( ">" ) ,
	MOVE_BACKWARD( "<" ) ,
	TURN_LEFT( "^" ) ,
	TURN_RIGHT( "v" ) ,
	TOUCH( "-" ) ,
	TOUCH_RIGHT( "\\" ) ,
	TOUCH_LEFT( "/" ) ;

	private final static Map<String , Move> BY_LABEL = new HashMap<String , Move>() ;
	private final String label ;

	static {
		for ( Move schema : Move.values() ) {
			BY_LABEL.put( schema.label , schema ) ;
		}
	}

	private Move( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}

	public static boolean isExist( String label ) {
		return BY_LABEL.containsKey( label ) ;
	}

	public static Move getByLabel( String label ) throws IllegalArgumentException {
		if ( BY_LABEL.containsKey( label ) ) {
			return BY_LABEL.get( label ) ;
		}

		throw new IllegalArgumentException() ;
	}
}
