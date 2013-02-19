package agent ;

public enum Schema {
	MOVE( ">" ) ,
	BACKWARD( "<" ),
	LEFT( "^" ) ,
	RIGHT( "v" ) ,
	TOUCH( "-" ) ,
	TOUCH_RIGHT( "\\" ) ,
	TOUCH_LEFT( "/" ) ;

	private String label ;

	private Schema( String label ) {
		this.label = label ;
	}

	public String getLabel() {
		return this.label ;
	}
	
	public static Schema valueOfByLabel( String label ) throws IllegalArgumentException{
		for ( Schema schema : Schema.values() ) {
			if( schema.getLabel().equalsIgnoreCase( label ) ){
				return schema;
			}
		}
		throw new IllegalArgumentException();
	}
}
