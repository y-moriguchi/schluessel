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
import java.text.SimpleDateFormat;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class MtimeFileFilter implements FileFilter {

	//
	static final SimpleDateFormat FORMAT;

	static {
		FORMAT = new SimpleDateFormat("yyyy/MM/dd");
		FORMAT.setLenient(false);
	}

	private String relop;
	private java.util.Date date;

	public MtimeFileFilter(String relop, java.util.Date date) {
		this.relop = relop;
		this.date  = date;
	}

	@Override
	public boolean accept(File pathname) {
		if(relop.equals("<")) {
			return pathname.lastModified() < date.getTime();
		} else if(relop.equals("<=")) {
			return pathname.lastModified() <= date.getTime();
		} else if(relop.equals(">")) {
			return pathname.lastModified() > date.getTime();
		} else if(relop.equals(">=")) {
			return pathname.lastModified() >= date.getTime();
		} else if(relop.equals("=")) {
			return pathname.lastModified() == date.getTime();
		} else if(relop.equals("!=")) {
			return pathname.lastModified() != date.getTime();
		} else {
			throw new RuntimeException();
		}
	}

	public String toString() {
		return ("mtime" + relop + FORMAT.format(date));
	}

}
