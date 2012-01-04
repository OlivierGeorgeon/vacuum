import java.awt.Color;
import java.awt.Graphics;

import javax.swing.JPanel;

/**
 * Panel used to display informations concerning colliculus
 * @author simon
 */
public class ColliculusPanel extends JPanel{
	
	public Colliculus colliculus;
	public int width;
	
	public int index;
	public Color[][] bundleTrace;
	
	public Color[][] bundleTraceRed;
	public Color[][] bundleTraceCyan;
	
	public ColliculusPanel(Colliculus c){
		colliculus=c;
		width=100/(40-10);
		index=0;
		bundleTrace=new Color[505][800];
		bundleTraceRed=new Color[505][800];
		bundleTraceCyan=new Color[505][800];
		
		for (int i=0;i<505;i++){
			for (int j=0;j<800;j++){
				bundleTrace[i][j]=Color.black;
				bundleTraceRed[i][j]=Color.black;
				bundleTraceCyan[i][j]=Color.black;
			}
		}
	}
	
	public void setColliculus(Colliculus c){
		colliculus=c;
	}
	
	
	public void paintComponent(Graphics g){
		
		g.setColor(Color.white);
		g.fillRect(0, 0, 685, 800);
		
		float x,y;

		int offsetX,offsetY;
		offsetX=160;
		offsetY=170;
		g.setColor(Color.black);
		g.drawString("tactile cartesian", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		g.setColor(Color.black);
		g.fillRect(offsetX-50*width, offsetY-50*width, 100*width, 100*width);		
		
		offsetX=160;
		offsetY=500;
		g.setColor(Color.black);
		g.drawString("visual cartesian", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		
		g.setColor(Color.black);
		g.fillRect(offsetX-50*width, offsetY-50*width, 100*width, 100*width);
		
		
		offsetX=500;
		offsetY=170;
		g.setColor(Color.black);
		g.fillRect(offsetX-50*width, offsetY-50*width, 100*width, 100*width);
		g.drawString("bundle map", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		/**/
		//===============================================================================
		float max=0;
		float max1=0,max2=0;
		int imax1=-1,imax2=-1;
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				//--------------------------------------------------------------
				// tactile colliculus in Cartesian referential
				offsetX=160;
				offsetY=170;

				g.setColor(new Color(Math.min(1,Math.max(colliculus.tmap.chargeMap1[i][j][2],colliculus.tmap.chargeMap1[i][j][3])),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][1]),
						             Math.min(1,colliculus.tmap.chargeMap1[i][j][0])) );

				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + j*width,
						   width, width);
				/* */
				
				//--------------------------------------------------------------
				// tactile colliculus in Cartesian referential (real colors)
				/*offsetX=160;
				offsetY=170;
				float max1=0,max2=0;
				int imax1=-1,imax2=-1;
				for (int i2=0;i2<4;i2++){
					if (colliculus.tmap.chargeMap1[i][j][i2]>max2){
						max2=colliculus.tmap.chargeMap1[i][j][i2];
						imax2=i2;
					}
				}
				if (max2>0.05){
					g.setColor(colliculus.bundleColor[0][imax2+1]);
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + j*width,
							   width, width);
				}
				/* */
				
				//---------------------------------------------------------------
				// visual colliculus in Cartesian referential
				offsetX=160;
				offsetY=500;
				max=0;
				for (int k=2;k<11;k++){
					if (colliculus.vmap.chargeMap1[i][j][k]>max) max=colliculus.vmap.chargeMap1[i][j][k];
				}
				g.setColor(new Color(max,
									 colliculus.vmap.chargeMap1[i][j][1],
									 colliculus.vmap.chargeMap1[i][j][0]));
				
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + j*width, 
						   width, width);
				/* */
				//--------------------------------------------------------------
				// visual colliculus in Cartesian referential (real colors)
				/*offsetX=160;
				offsetY=500;
				max1=0;
				max2=0;
				imax1=-1;
				imax2=-1;
				for (int i2=0;i2<10;i2++){
					if (colliculus.vmap.chargeMap1[i][j][i2]>max1){
						max1=colliculus.vmap.chargeMap1[i][j][i2];
						imax1=i2;
					}
				}
				if (max1>0.05){
					g.setColor(colliculus.bundleColor[imax1+1][0]);
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + j*width,
							   width, width);
				}
				/* */
				
				
				//---------------------------------------------------------------
				// bundle map
				/*offsetX=500;
				offsetY=170;
				max1=0;
				max2=0;
				imax1=-1;
				imax2=-1;
				for (int i2=0;i2<10;i2++){
					if (colliculus.vmap.chargeMap1[i][j][i2]>max1){
						max1=colliculus.vmap.chargeMap1[i][j][i2];
						imax1=i2;
					}
				}
				for (int i2=0;i2<4;i2++){
					if (colliculus.tmap.chargeMap1[i][j][i2]>max2){
						max2=colliculus.tmap.chargeMap1[i][j][i2];
						imax2=i2;
					}
				}
				if (max1>0.05 && max2>0.05 && colliculus.bundles[imax1][imax2]>0){
					g.setColor(colliculus.bundleColor[imax1+1][imax2+1]);
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + j*width,
							   width, width);
				}
				/* */
				
				//---------------------------------------------------------------
				// bundle map with speculation
				offsetX=500;
				offsetY=170;
				if (colliculus.bundleMapProba[i][j]>0.05){
					g.setColor(
						colliculus.bundleColor[colliculus.bundleMapType[i][j][0]+1][colliculus.bundleMapType[i][j][1]+1]);
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + j*width,
							   width, width);
				}
				
				/* */
				
			}
		}
		//Error intializing
		//---------------------------------------------------------------
		int color=9;
		int touch=2;
		int minIndex;
		// bundle map trace
		/*offsetX=690;
		offsetY=10;
		
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				float proba=colliculus.visualProba[i][99-j][color]*colliculus.tactileProba[i][99-j][touch];
				
				if (proba>0.1){
					for (int i2=0;i2<4;i2++){
						float red1=proba*(float)colliculus.bundleColor[color+1][touch+1].getRed();
						float red2=(1-proba)*(float)bundleTrace[i*4+i2+j][(799-index)-j].getRed();
						float green1=proba*(float)colliculus.bundleColor[color+1][touch+1].getGreen();
						float green2=(1-proba)*(float)bundleTrace[i*4+i2+j][(799-index)-j].getGreen();
						float blue1=proba*(float)colliculus.bundleColor[color+1][touch+1].getBlue();
						float blue2=(1-proba)*(float)bundleTrace[i*4+i2+j][(799-index)-j].getBlue();
					
						bundleTrace[i*4+i2+j][(799-index)-j]=new Color( (int)(red1+red2),(int)(green1+green2),(int)(blue1+blue2));
					}
				}
				
				if (  (i==0 || i==50 ||i==99) 
				    &&(j==0 || j==50 || j==99) ){
					bundleTrace[i*4+j][(799-index)-j]=Color.blue;
				}
				if (j==50 && i==50){
					bundleTrace[i*4+j][(799-index)-j]=Color.cyan;
				}
			}
		}
		
		minIndex=Math.min(799,810-index);
		for (int i=0;i<500;i++){
			for (int j=minIndex;j>699-index;j--){
				g.setColor(bundleTrace[i][j]);
				g.fillRect(offsetX + i,
						   offsetY + j,
						   1, 1);
			}
		}
		
		g.setColor(Color.red);
		//g.drawRect(offsetX+index*width-colliculus.tmap.mapSize/2*width, offsetY-colliculus.tmap.mapSize/2*width, 50*width, 100*width);
		g.drawLine(offsetX    , offsetY+(800-index),
				   offsetX+400, offsetY+(800-index));
		g.drawLine(offsetX+100, offsetY+(700-index),
				   offsetX+499, offsetY+(700-index));
		
		g.drawLine(offsetX    , offsetY+(800-index),
				   offsetX+100, offsetY+(700-index));
		g.drawLine(offsetX+400, offsetY+(800-index),
				   offsetX+499, offsetY+(700-index));
		
		index++;
		if (index>=700){
			index=0;
			for (int i=0;i<500;i++){
				for (int j=0;j<800;j++){
					bundleTrace[i][j]=Color.black;
				}
			}
		}
		/* */
		//***********************************************************************
		
		//---------------------------------------------------------------
		// bundle map trace (anaglyph)
		/*offsetX=690;
		offsetY=10;
		
		for (int i=0;i<100;i++){
			for (int j=0;j<100;j++){
				float proba=colliculus.visualProba[i][99-j][color]*colliculus.tactileProba[i][99-j][touch];
				
				if (proba>0.1){
					for (int i2=0;i2<4;i2++){
						float red1=proba*200;
						float red2=(1-proba)*(float)bundleTraceRed[(int) (i*4+i2+j*0.95)][(799-index)-j].getRed();
					
						float green1=proba*200;
						float green2=(1-proba)*(float)bundleTraceCyan[(int) (i*4+i2+j*1.05)][(799-index)-j].getGreen();
						float blue1=proba*200;
						float blue2=(1-proba)*(float)bundleTraceCyan[(int) (i*4+i2+j*1.05)][(799-index)-j].getBlue();
				
						bundleTraceRed[(int) (i*4+i2+j*0.95)][(799-index)-j]=new Color( (int)(red1+red2),0,0);
						bundleTraceCyan[(int) (i*4+i2+j*1.05)][(799-index)-j]=new Color( 0,(int)(green1+green2),(int)(blue1+blue2));
					}
				}
				
				if (  (i==0 || i==50 ||i==99) 
				    &&(j==0 || j==50 || j==99) ){
					bundleTraceRed[(int) (i*4+j*0.95)][(799-index)-j]=Color.LIGHT_GRAY;
					bundleTraceCyan[(int) (i*4+j*1.05)][(799-index)-j]=Color.LIGHT_GRAY;
				}

			}
		}

		minIndex=Math.min(799,810-index);
		for (int i=0;i<505;i++){
			for (int j=minIndex;j>699-index;j--){
				g.setColor(new Color(bundleTraceRed[i][j].getRed(),bundleTraceCyan[i][j].getGreen(),bundleTraceCyan[i][j].getBlue()));
				g.fillRect(offsetX + i,
						   offsetY + j,
						   1, 1);
			}
		}

		index++;
		if (index>=700){
			index=0;
			for (int i=0;i<505;i++){
				for (int j=0;j<800;j++){
					bundleTraceRed[i][j]=Color.black;
					bundleTraceCyan[i][j]=Color.black;
				}
			}
			g.fillRect(offsetX, offsetY, 505, 800);
		}
		/* */
		//***********************************************************************
		
		
		g.setColor(Color.red);
		g.fillOval(158, 168, 2*width,2*width );
		g.fillOval(158, 498, 2*width,2*width );
		g.fillOval(498, 168, 2*width,2*width );
		//=======================================================================
		
		
		
		
		
		///////////////////////////////////////////////////////////////////////
		// draw bundle connections
		offsetX=500;
		offsetY=500;
		g.setColor(Color.black);
		g.drawString("stimuli connections", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		offsetX=350;
		offsetY=400;
		g.setColor(Color.black);
		g.drawRect(offsetX-3, offsetY-3, 360, 25);
		g.drawRect(offsetX-3, offsetY-3, 25, 180);
		g.drawRect(offsetX-3, offsetY-3, 360, 180);
		
		for (int i=0;i<12;i++){
			for (int j=0;j<6;j++){
				if (i==0 || j==0){
					if (i<12 && j<6){
						g.setColor(colliculus.bundleColor[i][j]);
						g.fillRect(offsetX+i*30, offsetY+j*30, 20, 20);
					}
				}
				else{
					if (colliculus.bundles[i-1][j-1]==1){
						g.setColor(colliculus.bundleColor[i][j]);
						g.fillRect(offsetX+(i)*30, offsetY+(j)*30, 20, 20);
					}
				}
			}
		}
		/* */
		
		
		//============================================================================
		double Sum0,Sum1,countD,d;
		max=0;
		int red=0,green,blue;
		/*offsetX=500;
		offsetY=170;
		g.setColor(Color.black);
		g.drawString("tactile polar", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);
		offsetX=500;
		offsetY=500;
		g.setColor(Color.black);
		g.drawString("visual polar", offsetX-colliculus.tmap.mapSize/2*width+2,
				                          offsetY-colliculus.tmap.mapSize/2*width-2);*/
		
		for (int i=0;i<180;i++){
			for (int j=0;j<100;j++){
				//----------------------------------------------------------------
				// tactile charge map in Polar referential
				/*
				offsetX=500;
				offsetY=170;
				g.setColor(new Color( Math.min(1,colliculus.tmap.chargeMapP[i][j][2]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][1]),
									  Math.min(1,colliculus.tmap.chargeMapP[i][j][0])) );
					
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY-colliculus.tmap.mapSize/2*width + (100-j)*width,
						   width, width);
				/**/
				
				
				//--------------------------------------------------------------
				// visual colliculus in polar referential
				/*offsetX=500;
				offsetY=500;
				max=0;
				for (int k=2;k<10;k++){
					if (colliculus.vmap.chargeMapP[i][j][k]>max) max=colliculus.vmap.chargeMapP[i][j][k];
				}
				g.setColor(new Color(max,
									 colliculus.vmap.chargeMapP[i][j][1],
									 colliculus.vmap.chargeMapP[i][j][0]));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
				//--------------------------------------------------------------
				// tactile maximum charges in polar referential
				/*offsetX=500;
				offsetY=150;
				red=0;
				green=0;
				blue=0;
				if (colliculus.vmap.maximumMap[i][j]==0) blue=250;
				if (colliculus.vmap.maximumMap[i][j]==1) green=250;
				if (colliculus.tmap.maximumMap[i][j]==2) red=250;

				g.setColor(new Color(red,green,blue));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
				//--------------------------------------------------------------
				// visual maximum charges in polar referential
				/*offsetX=500;
				offsetY=500;
				max=0;
				red=0;
				green=0;
				blue=0;
				if (colliculus.vmap.maximumMap[i][j]==0) blue=250;
				if (colliculus.vmap.maximumMap[i][j]==1) green=250;
				if (colliculus.vmap.maximumMap[i][j]>1 ) red=250;
				
				g.setColor(new Color(red,green,blue));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width, 
						   width, width);
				/**/
				
			}
			
			
			//--------------------------------------------------------------
			// projection of the visual colliculus
			offsetX=500;
			offsetY=500;
			max=0;
			
			
			//--------------------------------------------------------------
			// draw tactile output vector
			/*offsetX=500;
			offsetY=150;
			g.setColor(Color.cyan);
			if (colliculus.tmap.output[i][0]>0){
				for (int j=colliculus.tmap.output[i][2];j<=colliculus.tmap.output[i][3];j++){
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY-colliculus.tmap.mapSize/2*width + (100-j)*width,
							   width, width);
				}
			}/**/
			
			//--------------------------------------------------------------
			// draw visual output vector
			/*offsetX=500;
			offsetY=500;
			g.setColor(Color.cyan);
			if (colliculus.vmap.output[i][0]>0){
				for (int j=colliculus.vmap.output[i][2];j<=colliculus.vmap.output[i][3];j++){
					g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
							   offsetY-colliculus.vmap.mapSize/2*width + (100-j)*width,
							   width, width);
				}
			}/**/
		}
		//================================================================
			
		
		//-----------------------------------------------------------------
		// projection of the tactile colliculus4
		/*
		offsetX=500;
		offsetY=170;
		
		g.setColor(Color.BLUE);
    	g.fillRect(offsetX-colliculus.tmap.mapSize/2*width,
    			   offsetY-150,180*width,150);
    	
    	g.setColor(Color.orange);
    	g.fillRect(offsetX-colliculus.tmap.mapSize/2*width,
    			   offsetY,180*width,150);
		for (int i=0;i<180;i++){
			if (colliculus.tmap.output[i][0]>0){
				int val=colliculus.tmap.output[i][0];
				if      (val==1) g.setColor(new Color(70,70,70));
				else if (val==2) g.setColor(new Color(200,200,200));
				else if (val==3) g.setColor(new Color(150,150,150));
				
				g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
						   offsetY - Math.min(100,1000/colliculus.tmap.output[i][1]),
						   width, 2*Math.min(100,1000/colliculus.tmap.output[i][1]));
			}
		}
		g.setColor(Color.red);
		g.drawLine(offsetX-colliculus.tmap.mapSize/2*width+45*width,
				   offsetY-150,
				   offsetX-colliculus.tmap.mapSize/2*width+45*width,
				   offsetY+150);
		
		g.drawLine(offsetX-colliculus.tmap.mapSize/2*width+135*width,
				   offsetY-150,
				   offsetX-colliculus.tmap.mapSize/2*width+135*width,
				   offsetY+150);
		/* */
		
		//-----------------------------------------------------------------
		// projection of the visual colliculus4
		/*
		offsetX=500;
		offsetY=500;
		
		g.setColor(Color.BLUE);
    	g.fillRect(offsetX-colliculus.vmap.mapSize/2*width,
    			   offsetY-150,180*width,150);
    	
    	g.setColor(Color.orange);
    	g.fillRect(offsetX-colliculus.vmap.mapSize/2*width,
    			   offsetY,180*width,150);
		for (int i=0;i<180;i++){
			if (colliculus.vmap.output[i][0]>0){
				int val=colliculus.vmap.output[i][0];
				if      (val==1) g.setColor(new Color(0,128,0));
				else if (val==2) g.setColor(new Color(115,230,0));
				else if (val==3) g.setColor(new Color(150,128,255));
				else if (val==4) g.setColor(new Color(46,230,0));
				else if (val==5) g.setColor(new Color(0,230,230));
				else if (val==6) g.setColor(new Color(0,230,92));
				else if (val==7) g.setColor(new Color(230,207,0));
				else if (val==8) g.setColor(new Color(0,230,161));
				else if (val==9) g.setColor(new Color(184,230,0));
				
				g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
						   offsetY - Math.min(100,1000/colliculus.vmap.output[i][1]),
						   width, 2*Math.min(100,1000/colliculus.vmap.output[i][1]));
			}
		}
		g.setColor(Color.red);
		g.drawLine(offsetX-colliculus.vmap.mapSize/2*width+45*width,
				   offsetY-150,
				   offsetX-colliculus.vmap.mapSize/2*width+45*width,
				   offsetY+150);
		
		g.drawLine(offsetX-colliculus.vmap.mapSize/2*width+135*width,
				   offsetY-150,
				   offsetX-colliculus.vmap.mapSize/2*width+135*width,
				   offsetY+150);
		/* */
		
		//---------------------------------------------------------------------
		// projection of bundles and stimuli
		/*offsetX=500;
		offsetY=170;
		g.setColor(Color.BLUE);
    	g.fillRect(offsetX-colliculus.tmap.mapSize/2*width,
    			   offsetY-150,180*width,150);
    	
    	g.setColor(Color.orange);
    	g.fillRect(offsetX-colliculus.tmap.mapSize/2*width,
    			   offsetY,180*width,150);
    	
    	for (int i=0;i<180;i++){
    		int val1=0,val2=0;
    		
			if (colliculus.tmap.output[i][0]>0){
				val1=colliculus.tmap.output[i][0];
			}
			
			if (colliculus.vmap.output[i][0]>0){
				val2=colliculus.vmap.output[i][0];
			}
			
			boolean bundle=false;
			if (val1>0 && val2>0){
				if (colliculus.bundles[val2][val1]==1){
					bundle=true;
					g.setColor(colliculus.bundleColor[val2+1][val1+1]);
					
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY - Math.min(100,1000/colliculus.vmap.output[i][1]),
							   width, 2*Math.min(100,1000/colliculus.vmap.output[i][1]));
				}
			}
			
			if (!bundle){
				if (val2>0){
					g.setColor(colliculus.bundleColor[val2+1][0]);
					g.fillRect(offsetX-colliculus.vmap.mapSize/2*width + i*width,
							   offsetY - Math.min(100,1000/colliculus.vmap.output[i][1]),
							   width, 2*Math.min(100,1000/colliculus.vmap.output[i][1]));
				}
				else if (val1>0){
					g.setColor(colliculus.bundleColor[0][val1+1]);
					g.fillRect(offsetX-colliculus.tmap.mapSize/2*width + i*width,
							   offsetY - Math.min(100,1000/colliculus.tmap.output[i][1]),
							   width, 2*Math.min(100,1000/colliculus.tmap.output[i][1]));
				}
			}
		}
		/* */
		
		/*
		colliculus.getSalienceList();

		g.setColor(Color.yellow);
		for (int i=0;i<colliculus.liste.size();i++){
			if (colliculus.liste.get(i).getType()==0){
				offsetX=500;
				offsetY=150;
				g.drawLine(offsetX-colliculus.vmap.mapSize/2*width 
					        	+ (int)(((colliculus.liste.get(i).getTheta()
					        	- colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
					       offsetY-colliculus.vmap.mapSize/2*width 
					   			+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width,
					   			
					       offsetX-colliculus.vmap.mapSize/2*width 
				        		+ (int)(((colliculus.liste.get(i).getTheta()
				        		+ colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
				           offsetY-colliculus.vmap.mapSize/2*width 
					   			+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width);
			}
			if (colliculus.liste.get(i).getType()==1){
				offsetX=500;
				offsetY=500;
				
				g.drawLine(offsetX-colliculus.vmap.mapSize/2*width 
					        	+ (int)(((colliculus.liste.get(i).getTheta()
					        	-colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
					       offsetY-colliculus.vmap.mapSize/2*width 
					       		+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width,
					   			
					       offsetX-colliculus.vmap.mapSize/2*width 
				        		+ (int)(((colliculus.liste.get(i).getTheta()
				        		+colliculus.liste.get(i).getSpanf()/2)*180/Math.PI+90)*width),
				           offsetY-colliculus.vmap.mapSize/2*width 
				           		+ (100-(int)(colliculus.liste.get(i).getDistancef()))*width);
			}
		}/**/
		
		
	}
}