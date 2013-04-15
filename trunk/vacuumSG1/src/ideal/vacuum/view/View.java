package ideal.vacuum.view;

import agent.controller.Controller ;


public interface View{
	
	public Controller getController();

	public void display();

	public void close();

	public boolean isDisplay();

	public void resetDisplay();
}
