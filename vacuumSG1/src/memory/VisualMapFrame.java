package memory;
import javax.swing.JFrame;


public class VisualMapFrame extends JFrame{

	private VisualMapPanel panel;
	
	public VisualMapFrame(VisualMap v){
		this.setTitle("visual Map");
    	this.setSize(800, 800);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new VisualMapPanel(v);
    	
    	this.setContentPane(panel);
	}
	
	public void setVisual(VisualMap v){
		panel.setVisual(v);
	}
	
	public void paint(){
    	panel.repaint();
    }
}
