package agent;

import java.awt.Graphics;

import javax.swing.JPanel;

public class PrintablePanel extends JPanel{

	
	public PrintablePanel(){

	}

	public void drawPDF(Graphics g){
		paintComponent(g);
	}
	
	public void paintComponent(Graphics g){

	}
	
}
