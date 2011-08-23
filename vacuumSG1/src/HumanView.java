

import java.awt.event.KeyEvent;
import java.util.LinkedList;
import java.util.Queue;

import javax.swing.JOptionPane;

public class HumanView implements Runnable {
	private final Model m_model;

	private Queue<KeyEvent> queryHuman;

	private LogData humanLog;

	private long startTime;

	private long endTime;

	public HumanView(Model modelInput) {
		m_model = modelInput;
		queryHuman = new LinkedList<KeyEvent>();
		humanLog = new LogData(modelInput);
		startTime = 0;
		endTime = 0;
	}

	public void init() {
		m_model.resetStepCount();
		m_model.resetScore();
		m_model.haltAgent();
	}

	protected void preStep() {
		m_model.tallyScore();
		System.out.println("Step #" + m_model.getStepCount());
		m_model.decStepCount();
	}

	public void run() {

		m_model.startAgent();
		humanLog.setFileToPrint(m_model.getLogFile());
		
		try {

			startTime = System.currentTimeMillis();

			while ((m_model.getStepCount() > 0) && !m_model.isAgentStopped()) {

				if (!queryHuman.isEmpty()) {
					while (!queryHuman.isEmpty()) {
						preStep();
						handleAction(queryHuman.poll());
					}
				}
				
				try {
					Thread.sleep(10);
				} catch (InterruptedException ue) {
					System.out.println(ue);
				}
//				if the programm should stop when the is no more to suck
//				boolean isNotFinish = false;
//				for (int y = 0; y < m_model.getHeight(); y++) {
//					for (int x = 0; x < m_model.getWidth(); x++) {
//						if(m_model.isDirty(x, y)) isNotFinish = true;		
//					}
//				}
//				if (!isNotFinish) m_model.haltAgent();
			}
			
			endTime = System.currentTimeMillis();

			humanLog.setDuration(endTime-startTime);
			humanLog.saveLog();
			
			//JOptionPane.showMessageDialog(Main.MAIN_WIN, "Final Score: "
			//		+ m_model.getScore(), "Score",
			//		JOptionPane.INFORMATION_MESSAGE);

			
			init();

		} finally {
			m_model.haltAgent();
		}
	}

	private void handleAction(KeyEvent e) {
		switch (e.getKeyCode()) {
		case 32:
			if (m_model.isDirty(m_model.agentX(), m_model.agentY()))
				humanLog.getMoveRecord().put(
						System.currentTimeMillis() - startTime, "Suck\t1");
			else
				humanLog.getMoveRecord().put(
						System.currentTimeMillis() - startTime, "Suck\t0");

			m_model.suck();
			break;

		case 37:
			humanLog.getMoveRecord().put(
					System.currentTimeMillis() - startTime, "Left");
			m_model.left();
			break;

		case 38:
			humanLog.getMoveRecord().put(
					System.currentTimeMillis() - startTime, "Up");
			m_model.up();
			break;

		case 39:
			humanLog.getMoveRecord().put(
					System.currentTimeMillis() - startTime, "Right");
			m_model.right();
			break;

		case 40:
			humanLog.getMoveRecord().put(
					System.currentTimeMillis() - startTime, "Down");
			m_model.down();
			break;
		}
	}

	public Queue<KeyEvent> getQueryHuman() {
		return queryHuman;
	}

}