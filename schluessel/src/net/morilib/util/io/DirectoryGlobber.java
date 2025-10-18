/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.util.io;

import java.io.File;
import java.io.FileFilter;
import java.util.HashSet;
import java.util.Iterator;
import java.util.NoSuchElementException;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.util.io.filter.AndFileFilter;
import net.morilib.util.io.filter.WildcardFileFilter;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public final class DirectoryGlobber {
	
	//
	static class DirectoryIterator implements Iterator<File> {

		//
		private File[] files;
		private int ptr = 0;

		/**
		 * 
		 * @param file
		 */
		public DirectoryIterator(File file) {
			files = file.listFiles();
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#hasNext()
		 */
		@Override
		public boolean hasNext() {
			return files != null && ptr < files.length;
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#next()
		 */
		@Override
		public File next() {
			if(!hasNext())  throw new NoSuchElementException();
			return files[ptr++];
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#remove()
		 */
		@Override
		public void remove() {
			throw new UnsupportedOperationException();
		}

	}

	//
	private static String[] compile(String pattern) {
		String[] r;

		if(pattern.matches("/+$")) {
			throw new IllegalArgumentException();
		}

		r = pattern.split("/+");
		for(int i = 0; i < r.length; i++) {
			if(r[i].equals("**"))  r[i] = null;
		}
		return r;
	}

	//
	private static final Pattern COMPPTN =
			Pattern.compile("^(.*?)(<.*>)?$");

	//
	private static FileFilter[] compfilt(String[] ptn) {
		Matcher mt;
		FileFilter[] r = new FileFilter[ptn.length];
		FileFilter f1;
		String x;

		for(int i = 0; i < ptn.length; i++) {
			if(ptn[i] == null) {
				r[i] = null;
			} else {
				mt = COMPPTN.matcher(ptn[i]);
				if(!mt.matches())  throw new RuntimeException();
				f1 = new WildcardFileFilter(mt.group(1));
				if((x = mt.group(2)) == null) {
					r[i] = f1;
				} else {
					x = x.substring(1, x.length() - 1);
					r[i] = new AndFileFilter(
							f1, FileAttributeFinder.compile(x));
				}
			}
		}
		return r;
	}

	//
	private static void glob1(Set<File> r, File file, FileFilter[] ptn,
			int ptr) {
		Iterator<File> itr;
		File f;

		if(ptr >= ptn.length)  return;
		itr = new DirectoryIterator(file);
		while(itr.hasNext()) {
			f = itr.next();
			if(ptn[ptr] == null) {
				if(f.isDirectory())  glob1(r, f, ptn, ptr);
				glob1(r, file, ptn, ptr + 1);
			} else if(ptn[ptr].accept(f)) {
				if(f.isDirectory()) {
					glob1(r, f, ptn, ptr + 1);
				} else if(ptr + 1 == ptn.length) {
					r.add(f);
				}
			}
		}
	}

	/**
	 * 
	 * @param f
	 * @param pattern
	 * @return
	 */
	public static File[] glob(File f, String pattern) {
		Set<File> r = new HashSet<File>();

		glob1(r, f, compfilt(compile(pattern)), 0);
		return r.toArray(new File[0]);
	}

	/**
	 * 
	 * @param pattern
	 * @return
	 */
	public static File[] glob(String pattern) {
		String[] p = compile(pattern);
		Set<File> r = new HashSet<File>();

		if(p.length > 0 && p[0].isEmpty()) {
			glob1(r, new File("/"), compfilt(p), 1);
		} else {
			glob1(r, new File("."), compfilt(p), 0);
		}
		return r.toArray(new File[0]);
	}

}
