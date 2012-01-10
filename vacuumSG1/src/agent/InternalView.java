package agent;

import java.awt.Color;

public class InternalView {

	
	double[] retine;
	double[] tactile;
	Color[] colorMap;
	int[] tactileMap;
	int[] corner;
	int[] corner2;
	
	int left,right;
	
	public InternalView(){
		
	}
	
	public void updateRetine(double[] r,Color[] cm,int[] cor,double[] rt,int[] cmt,int[] cort){
    	retine=r;
    	colorMap=cm;
    	corner=cor;
    	
    	tactile=rt;
    	tactileMap=cmt;
    	corner2=cort;
    }
}
