package net.morilib.util.io.filter;

import java.io.File;
import java.io.FileFilter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class AndFileFilter implements FileFilter {

	private List<FileFilter> ex;

	/**
	 * 
	 * @param ex
	 */
	public AndFileFilter(List<FileFilter> ex) {
		this.ex = new ArrayList<FileFilter>(ex);
	}

	/**
	 * 
	 * @param filters
	 */
	public AndFileFilter(FileFilter... filters) {
		this(Arrays.asList(filters));
	}

	@Override
	public boolean accept(File pathname) {
		for(FileFilter f : ex) {
			if(!f.accept(pathname))  return false;
		}
		return true;
	}

	public String toString() {
		StringBuilder b = new StringBuilder("(");
		String s = "";

		for(FileFilter f : ex) {
			b.append(s).append(f.toString());
			s = "&";
		}
		return b.append(")").toString();
	}

}
