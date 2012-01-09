package memory110;

import java.util.ArrayList;

import spas.IPlace;

public class SpaceMemory {

	public ArrayList<IPlace> placeList;
	
	public SpaceMemory(){
		
	}
	
	public void update(ArrayList<IPlace> list){
		placeList=list;
	}
	
}
