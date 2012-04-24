package agent;

import java.awt.Graphics;
import java.awt.Graphics2D;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.print.PageFormat;
import java.awt.print.Printable;
import java.awt.print.PrinterException;
import java.awt.print.PrinterJob;

import javax.swing.JFrame;

public class PrintableFrame extends JFrame implements Printable, KeyListener{

	
	public PrintablePanel panel;
	
	public PrintableFrame(){
		addKeyListener(this);
		
		panel=new PrintablePanel();
	}
	
	public void saveImage(){

	}

	
	public void drawPDF(Graphics g){
		panel.drawPDF(g);
	}
	
	
	@Override
	public void keyPressed(KeyEvent e) {
			if (e.isControlDown() && e.getKeyCode()==80 ){

				PrinterJob job = PrinterJob.getPrinterJob();

				job.setPrintable(this);
				boolean ok = job.printDialog();
				if (ok) {
					try {
						job.print();
					} catch (PrinterException ex) {
						/* The job did not successfully complete */
					}
				}
			}		
	}

	@Override
	public void keyReleased(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void keyTyped(KeyEvent arg0) {
		// TODO Auto-generated method stub
		
	}

	public int print(Graphics g, PageFormat pf, int page) throws PrinterException {
		
		 // We have only one page, and 'page'
	    // is zero-based
	    if (page > 0) {
	         return NO_SUCH_PAGE;
	    }
	    Graphics2D g2d = (Graphics2D)g;
	    g2d.translate(pf.getImageableX(), pf.getImageableY());
	    
	    // Now we perform our rendering
	    drawPDF(g);

	    // tell the caller that this page is part
	    // of the printed document
	    return PAGE_EXISTS;
	}

	
	
}
