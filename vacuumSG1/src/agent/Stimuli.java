package agent ;

public enum Stimuli {
	FALSE( "f" ) ,
	TRUE( "t" ) ,
	BRICK( "b" ),
	ALGA( "a" );

	private String label ;

	private Stimuli( String pLabel ) {
		this.label = pLabel ;
	}

	public String getLabel() {
		return this.label ;
	}
}
