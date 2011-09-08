import java.awt.Color;
import java.util.ArrayList;


// PaternMap save and show the repartition of visual patterns on a map
public class PatternMap {

	
	public ArrayList<ArrayList<Color>> patternList;     // list of all patterns
	public ArrayList<Color> actualPattern;				// pattern actually seen by Ernest
	public int actualIndex;
	public int actualNode;
	public int previousNode;
	public ArrayList<Color> patternColor;				// color to identify patterns
	
	public ArrayList<PatternNode> nodeList;
	
	
	public PatternMap(){
		patternList=new ArrayList<ArrayList<Color>>();
		actualPattern=new ArrayList<Color>();
		actualIndex=0;
		actualNode=-1;
		previousNode=-1;
		patternColor=new ArrayList<Color>();

		nodeList=new ArrayList<PatternNode>();
	}
	
	
	public void moveNodes(){
		float fx,fy;
		// n iterations (n=30)
		for (int k=0;k<30;k++){
			for (int i=0;i<nodeList.size();i++){
				fx=0;
				fy=0;
				// walls repulsion force
				if (nodeList.get(i).x<100) fx+= 2/nodeList.get(i).x;
				if (nodeList.get(i).x>400) fx+= 2/(nodeList.get(i).x-500);
				if (nodeList.get(i).y<100) fy+= 2/nodeList.get(i).y;
				if (nodeList.get(i).y>400) fy+= 2/(nodeList.get(i).y-500);
				// other nodes repulsion
				for (int j=0;j<nodeList.size();j++){
					if (i!=j && Math.sqrt( (nodeList.get(i).x-nodeList.get(j).x) * (nodeList.get(i).x-nodeList.get(j).x)
										 + (nodeList.get(i).y-nodeList.get(j).y) * (nodeList.get(i).y-nodeList.get(j).y) ) <100){
						fx+= 1/ (nodeList.get(i).x-nodeList.get(j).x);
						fy+= 1/ (nodeList.get(i).y-nodeList.get(j).y);
					}
				}
				// link attraction force
				for (int j=0;j<nodeList.get(i).output.size();j++){
					fx+= (nodeList.get(nodeList.get(i).output.get(j)).x- nodeList.get(i).x)/1000;
					fy+= (nodeList.get(nodeList.get(i).output.get(j)).x- nodeList.get(i).x)/1000;
				}
				nodeList.get(i).x=Math.max(0, Math.min(500,nodeList.get(i).x+fx));
				nodeList.get(i).y=Math.max(0, Math.min(500,nodeList.get(i).y+fy));
			}
		}
	}
	
	
	public void addPatern(Color[] colorMap,int lastAction){
		
		Color color;
		ArrayList<Color> newPattern=new ArrayList<Color>();
		
		color=colorMap[90];
		newPattern.add(color);
		
		int red=0;
		int green=0;
		int blue=0;
		
		// create the actual visual pattern
		
		// too many patterns created with this version
		/*for (int i=1;i<colorMap.length;i++){
			if (!colorMap[i].equals(color)){
				color=colorMap[i];
				newPattern.add(color);
			}
		}*/
		for (int i=1;i<colorMap.length;i++){
			if (newPattern.indexOf(colorMap[i])==-1){
				red=colorMap[i].getRed();
				green=colorMap[i].getGreen();
				int j=1;
				while (j<newPattern.size() && 
							   (red< newPattern.get(j).getRed()  || 
							   (red==newPattern.get(j).getRed() && green<newPattern.get(j).getGreen()) ) ){
					j++;
				}
				newPattern.add(j,colorMap[i]);
			}
		}
		
		// test if the pattern already exist
		int index=patternList.indexOf(newPattern);
		
		// if the pattern is new
		if (index==-1) {
			// add it to the list
			patternList.add(newPattern);
			
			// save the pattern as the actual
			actualPattern=patternList.get(patternList.size()-1);
			actualIndex=patternList.size()-1;
			
			// define a new color to represent it
			int count=0;
			index=-1;
			Color newColor=new Color(0,0,0);
			while (index==-1 && count<5){
				red=  (int) (Math.random()*255);
				green=(int) (Math.random()*255);
				blue= (int) (Math.random()*255);
				newColor= new Color(red,green,blue);
				index=patternColor.indexOf(newColor);
				count++;
			}
			patternColor.add(newColor);
			
			
			
			// a new pattern means a new node
			nodeList.add(new PatternNode(nodeList.size(),250,250));
			// choose a position around the previous one
			if (!nodeList.isEmpty() && actualNode!=-1){
				nodeList.get(nodeList.size()-1).setPosition((float)(nodeList.get(actualNode).x+Math.random()*30-15),
															(float)(nodeList.get(actualNode).y+Math.random()*30-15));
			}
			
			// save the new node as an output for the previous node and the previous node as an input for the new one,
			if (actualNode!=-1){
				nodeList.get(actualNode).addOutput(nodeList.size()-1);
			}
				
				
			// save the new node as the actual
			previousNode=actualNode;
			actualNode=nodeList.size()-1;
			
			moveNodes();
			
		}
		// if the pattern already exist
		else{
			// save the pattern as the actual
			actualPattern=patternList.get(index);
			actualIndex=index;
			
			
			// if the pattern changed, save input and output node and connection
			if (actualNode!=index && actualNode!=-1){
				nodeList.get(actualNode).addOutput(index);
				moveNodes();
			}
			
			// save the new node as the actual
			if (index!=actualNode){
				previousNode=actualNode;
				actualNode=index;
			}
		}
		
	}
	
}
