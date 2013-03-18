package agent.model ;

public enum Stimuli {
	FALSE( "f" ) ,
	TRUE( "t" ) ,
	BRICK( "b" ) ,
	ALGA( "a" ) ,
	APPEAR( "*" ) ,
	CLOSER( "+" ) ,
	DISAPPEAR( "o" ) ,
	UNCHANGED( " " ) ;

	private String label ;

	private Stimuli( String pLabel ) {
		this.label = pLabel ;
	}

	public String getLabel() {
		return this.label ;
	}
}
