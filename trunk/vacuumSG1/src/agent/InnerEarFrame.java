package agent;
import javax.swing.JFrame;


public class InnerEarFrame extends JFrame{

	private InnerEarPanel panel;
	
	public InnerEarFrame(InnerEar i){
		this.setTitle("Inner ear");
    	this.setSize(400, 500);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new InnerEarPanel(i);
    	
    	this.setContentPane(panel);
	}
	
	public void setInnerEar(InnerEar i){
		panel.setInnerEar(i);
	}
	
	public void paint(){
    	panel.repaint();
    }
}
