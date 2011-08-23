

import java.io.File;
import javax.swing.filechooser.FileFilter;

public class MyFileFilter extends FileFilter
{
	private String m_filter;
	private String m_desc;

	public MyFileFilter()
	{}

	public void set(String filter, String desc)
	{
		m_desc = desc;
		m_filter = filter;
	}

	public boolean accept(File f) 
	{
		String name = f.getName().toUpperCase();
		if (name != null && m_filter != null)
			return f.isDirectory() ||  name.endsWith(m_filter);
		else
			return false;
	}

	public String getDescription() 
	{ return m_desc; }
}
