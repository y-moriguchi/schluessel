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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/22
 */
public class NotFileFilter implements FileFilter {

	private FileFilter ex1;

	public NotFileFilter(FileFilter e1) {
		ex1 = e1;
	}

	@Override
	public boolean accept(File pathname) {
		return !ex1.accept(pathname);
	}

	public String toString() {
		return "!" + ex1.toString();
	}

}
