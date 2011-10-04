import javax.swing.JFrame;


public class TactileMapFrame extends JFrame{

	private TactileMapPanel panel;
	
	public TactileMapFrame(TactileMap t){
		this.setTitle("tactile Map");
    	this.setSize(800, 800);
    	this.setLocationRelativeTo(null);               
    	this.setVisible(true);

    	panel=new TactileMapPanel(t);
    	
    	this.setContentPane(panel);
	}
	
	
	public void paint(){
    	panel.repaint();
    }
}
