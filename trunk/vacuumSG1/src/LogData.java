

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

public class LogData {

	/**
	 * 
	 */
	private static final long serialVersionUID = 1L;

	private TreeMap<Long, String> moveRecord = null;

	private Long duration = null;

	private File fileToPrint = null;

	private Model m_Model = null;

	private String logFile;
	
	public LogData(Model m_ModelInput) {
		this.moveRecord = new TreeMap<Long, String>();
		this.m_Model = m_ModelInput;
		logFile = new String("VC"
				+ new SimpleDateFormat("yyyyMMMWWHHmmss").format(new Date(
						System.currentTimeMillis())) + ".txt");
	} 

	
	public void printToFile() {

		try {
			FileWriter writer = new FileWriter(fileToPrint, true);

			printHeadToFile(writer);
			printBodyToFile(writer);

			writer.close();

		} catch (FileNotFoundException e) {

			e.printStackTrace();
		} catch (IOException e) {

			e.printStackTrace();
		}
	}

	/**
	 * Prints a line to the log file 
	 * @author ogeorgeon 
	 */
	public void logLine(String Line) {

		try {
			FileWriter writer = new FileWriter(fileToPrint, true);

			writer.write(Line + "\n");

			writer.close();

		} catch (FileNotFoundException e) {

			e.printStackTrace();
		} catch (IOException e) {

			e.printStackTrace();
		}
	}

	// printing all the headinformation to the file;
	private void printHeadToFile(FileWriter writer) {
		try {
			writer.write("#"
					+ new SimpleDateFormat("MMMMM.dd.yyyy hh:mm aaa").format(
							new Date(System.currentTimeMillis())).toString());
			writer.write("\n#" + "size of board: " + m_Model.getHeight() + "x"
					+ m_Model.getWidth() + " / ");
			writer.write("number of dirty squares: " + m_Model.getDirtyCount()
					+ " / ");
			//writer.write("type of agent: " + m_Model.getType() + " / ");
			switch (m_Model.getType())
			{	case 1:
				{	writer.write("type of agent: Jess / ");
					break;
				}case 2:{
					writer.write("type of agent: Soar / ");
					break;
				}case 3:{
					writer.write("type of agent: Human / ");
					break;	
				}
			}
			writer.write("number of time steps: "
					+ (m_Model.getTotalSteps() - m_Model.getStepCount()));

		} catch (IOException e) {
			e.printStackTrace();
		}

	}

	// printing the actual data
	private void printBodyToFile(FileWriter writer) {
		try {
			writer.write("\n");
			for (Map.Entry<Long, String> iterator : moveRecord.entrySet()) {
				String time = new SimpleDateFormat("ssss.SSS").format(
						new Date(iterator.getKey())).toString();
				writer.write("\n" + time + "\t" + iterator.getValue());
			}
			writer.write("\n\n");
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	public void setFileToPrint(String fileNameOutput) {
		this.fileToPrint = new File(fileNameOutput);
	}

	public Long getDuration() {
		return duration;
	}

	public String getFormatedDuration() {
		return new SimpleDateFormat("ssss.SSS").format(new Date(duration))
				.toString();
	}

	public void setDuration(Long duration) {
		this.duration = duration;
	}

	public TreeMap<Long, String> getMoveRecord() {
		return moveRecord;
	}

	public void TreeMap(TreeMap<Long, String> moveRecord) {
		this.moveRecord = moveRecord;
	}

	public void saveLog() {
		this.printToFile();
		moveRecord.clear();
	}
}
