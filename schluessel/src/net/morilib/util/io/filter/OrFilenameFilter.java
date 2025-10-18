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
import java.io.FilenameFilter;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/29
 */
public class OrFilenameFilter implements FilenameFilter {

	//
	private FilenameFilter[] filters;

	/**
	 * 
	 * @param filters
	 */
	public OrFilenameFilter(FilenameFilter... filters) {
		this.filters = new FilenameFilter[filters.length];
		System.arraycopy(
				filters, 0, this.filters, 0, filters.length);
	}

	/* (non-Javadoc)
	 * @see java.io.FilenameFilter#accept(java.io.File, java.lang.String)
	 */
	public boolean accept(File dir, String name) {
		for(FilenameFilter f : filters) {
			if(f.accept(dir, name)) {
				return true;
			}
		}
		return false;
	}

}
