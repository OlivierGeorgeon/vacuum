package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum Schema {
	MOVE_FORWARD( ">" ) ,
	MOVE_BACKWARD( "<" ) ,
	TURN_LEFT( "^" ) ,
	TURN_RIGHT( "v" ) ,
	TOUCH( "-" ) ,
	TOUCH_RIGHT( "\\" ) ,
	TOUCH_LEFT( "/" ) ;

	private final static Map<String , Schema> BY_LABEL = new HashMap<String , Schema>() ;
	private final String label ;

	static {
		for ( Schema schema : Schema.values() ) {
			BY_LABEL.put( schema.label , schema ) ;
		}
	}

	private Schema( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}

	public static boolean isExist( String label ) {
		return BY_LABEL.containsKey( label ) ;
	}

	public static Schema getByLabel( String label ) throws IllegalArgumentException {
		if ( BY_LABEL.containsKey( label ) ) {
			return BY_LABEL.get( label ) ;
		}

		throw new IllegalArgumentException() ;
	}
}
