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
public class OrFileFilter implements FileFilter {

	private List<FileFilter> ex;

	/**
	 * 
	 * @param ex
	 */
	public OrFileFilter(List<FileFilter> ex) {
		this.ex = new ArrayList<FileFilter>(ex);
	}

	/**
	 * 
	 * @param filters
	 */
	public OrFileFilter(FileFilter... filters) {
		this(Arrays.asList(filters));
	}

	@Override
	public boolean accept(File pathname) {
		for(FileFilter f : ex) {
			if(f.accept(pathname))  return true;
		}
		return false;
	}

	public String toString() {
		StringBuilder b = new StringBuilder("(");
		String s = "";

		for(FileFilter f : ex) {
			b.append(s).append(f.toString());
			s = "|";
		}
		return b.append(")").toString();
	}

}
