package agent;

import java.awt.Color;
import java.util.ArrayList;

import javax.vecmath.Vector3f;

import memory110.Point;
import memory110.Segment;

public class InternalView {

	
	double[] retine;
	double[] tactile;
	Color[] colorMap;
	int[] tactileMap;
	int[] corner;
	int[] corner2;
	ArrayList<Point> cornerPoints;
	ArrayList<Segment> segments;
	
	int left,right;
	
	public InternalView(){
		retine=new double[360];
    	colorMap=new Color[360];
    	corner=new int[360];
    	
    	tactile=new double[360];
    	tactileMap=new int[360];
    	corner2=new int[360];
    	
    	segments=new ArrayList<Segment>();
    	
    	cornerPoints=new ArrayList<Point>();

	}
	
	public void updateRetine(double[] r,Color[] cm,int[] cor,double[] rt,int[] cmt,int[] cort,
			                 ArrayList<Point> cornerList, ArrayList<Segment> segmentList){
    	retine=r;
    	colorMap=cm;
    	corner=cor;
    	
    	tactile=rt;
    	tactileMap=cmt;
    	corner2=cort;
    	
    	cornerPoints=cornerList;
    	
    	segments=segmentList;
    }
}
