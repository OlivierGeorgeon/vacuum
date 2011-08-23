

import java.awt.BorderLayout;
import java.io.File;
import java.io.IOException;
import java.lang.reflect.Method;

import javax.swing.JEditorPane;
import javax.swing.JFrame;
import javax.swing.JOptionPane;
import javax.swing.event.HyperlinkEvent;
import javax.swing.event.HyperlinkListener;

@SuppressWarnings("serial")
public class HelpFrames extends Dialog implements HyperlinkListener {

	private JEditorPane htmlPane;
	
	public HelpFrames(JFrame parent, String type) {
		super(parent, "Key Stroke Short Cuts", false);

		if (type.equals("Keyboard")) {
			initComponentsKey();
		} else if (type.equals("About")) {

			initComponentsAbout();
		}
		this.setVisible(true);
	}

	private void initComponentsAbout() {
		String fileName = "AboutVacuum.html";

		try {

			if (getClass().getResource("/" + fileName) != null) {
				htmlPane = new JEditorPane(getClass().getResource(
						"/" + fileName).toString());
			} else {
				
				File htmlFile = new File(fileName);
				htmlPane = new JEditorPane(htmlFile.toURL());
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}

		htmlPane.setEditable(false);
		htmlPane.addHyperlinkListener(this);

		this.setTitle("About Vacuumcleaner ...");
		this.setName("");
		this.getContentPane().add(htmlPane, BorderLayout.CENTER);
		this.setSize(600, 600);
	}

	private void initComponentsKey() {

		String fileName = "KeyCuts.html";

		try {

			if (getClass().getResource("/" + fileName) != null) {
				htmlPane = new JEditorPane(getClass().getResource(
						"/" + fileName).toString());
			} else {
				
				File htmlFile = new File(fileName);
				htmlPane = new JEditorPane(htmlFile.toURL());
			}

		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		htmlPane.setEditable(false);
		
		setTitle("Key Stroke Short Cuts");
		setName("");

		this.getContentPane().add(htmlPane, BorderLayout.CENTER);
		this.setSize(300, 750);

	}

	@Override
	protected boolean handleOk() {
		// TODO Auto-generated method stub
		return false;
	}

	public void hyperlinkUpdate(HyperlinkEvent e) {
		if (e.getEventType() == javax.swing.event.HyperlinkEvent.EventType.ACTIVATED) {
			openURL(e.getURL().toString());
		}
	}

	private void openURL(String url) {
		String osName = System.getProperty("os.name");
		final String errMsg = "Error attempting to launch web browser";
		try {
			if (osName.startsWith("Mac OS")) {
				Class fileMgr = Class.forName("com.apple.eio.FileManager");
				Method openURL = fileMgr.getDeclaredMethod("openURL",
						new Class[] { String.class });
				openURL.invoke(null, new Object[] { url });
			} else if (osName.startsWith("Windows"))
				Runtime.getRuntime().exec(
						"rundll32 url.dll,FileProtocolHandler " + url);
			else {
				// assume Unix or Linux
				String[] browsers = { "firefox", "opera", "konqueror",
						"epiphany", "mozilla", "netscape" };
				String browser = null;
				for (int count = 0; count < browsers.length && browser == null; count++)
					if (Runtime.getRuntime().exec(
							new String[] { "which", browsers[count] })
							.waitFor() == 0)
						browser = browsers[count];
				if (browser == null)
					throw new Exception("Could not find web browser");
				else
					Runtime.getRuntime().exec(new String[] { browser, url });
			}
		} catch (Exception e) {
			JOptionPane.showMessageDialog(null, errMsg + ":\n"
					+ e.getLocalizedMessage());
		}
	}

	@Override
	protected void handleDefault() {
		// TODO Auto-generated method stub
		
	}

}
