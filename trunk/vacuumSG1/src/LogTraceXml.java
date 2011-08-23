
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import java.util.TreeMap;

public class LogTraceXml {

	/**
	 * Logs trace into xml file
	 * @author ogeorgeon 
	 */

	private File logFile = null;

	public LogTraceXml(String logFileName) {
		
		try 
		{
			logFile = new File(logFileName);
			if (logFile.exists()) 
			{
				logFile.delete();
				logFile.createNewFile();
			}
			logLine("<?xml version='1.0'?>");
			logLine("<?xml-stylesheet href='trace.xsl' type='text/xsl'?>");
			logLine("<xml>");
		}
		catch (IOException e) {
			e.printStackTrace();
		}
	} 

	
	/**
	 * Prints a line to the log file 
	 * @author ogeorgeon 
	 */
	public void logLine(String Line) {

		try {
			FileWriter writer = new FileWriter(logFile, true);

			writer.write(Line + "\n");

			writer.close();

		} catch (FileNotFoundException e) {

			e.printStackTrace();
		} catch (IOException e) {

			e.printStackTrace();
		}
	}
}
