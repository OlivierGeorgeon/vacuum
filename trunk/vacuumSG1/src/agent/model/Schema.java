package agent.model ;

import java.util.HashMap ;
import java.util.Map ;

public enum Schema {
	MOVE_FORWARD( ">" ) ,
	MOVE_BACKWARD( "<" ),
	TURN_LEFT( "^" ) ,
	TURN_RIGHT( "v" ) ,
	TOUCH( "-" ) ,
	TOUCH_RIGHT( "\\" ) ,
	TOUCH_LEFT( "/" ) ;

	private final static Map<String, Schema> BY_SIGN = new HashMap<String , Schema>();
	private final String sign ;

	static {
        for (Schema schema : Schema.values()) {
        	Schema.BY_SIGN.put(schema.sign, schema);
        }
    }
	
	private Schema( String sign ) {
		this.sign = sign ;
	}

	public String getSign() {
		return this.sign ;
	}
	
	public static Schema getBySign( String sign ) throws IllegalArgumentException{
		if( Schema.BY_SIGN.containsKey( sign ) ){
			return Schema.BY_SIGN.get( sign );
		}
		
		throw new IllegalArgumentException();
	}
}
