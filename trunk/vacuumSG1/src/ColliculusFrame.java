import javax.swing.JFrame;


public class ColliculusFrame extends JFrame{

	private ColliculusPanel panel;
	
	public ColliculusFrame(Colliculus c){
		this.setTitle("Colliculus");
    	this.setSize(1000, 800);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new ColliculusPanel(c);
    	
    	this.setContentPane(panel);
	}
	
	
	public void paint(){
    	panel.repaint();
    }
}