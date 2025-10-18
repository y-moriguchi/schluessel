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
package net.morilib.util.swing;

import java.io.File;
import java.util.Arrays;

import javax.swing.filechooser.FileFilter;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/19
 */
public class ExtensionFileFilter extends FileFilter {

	//
	private String[] extensions;
	private String   description;

	/**
	 * 
	 * @param extension
	 * @param description
	 */
	public ExtensionFileFilter(String extension, String description) {
		this.extensions  = extension.split(",");
		this.description = description;
	}

	/* (non-Javadoc)
	 * @see javax.swing.filechooser.FileFilter#accept(java.io.File)
	 */
	@Override
	public boolean accept(File f) {
		if(f.isDirectory()) {
			return true;
		}

		for(int i = 0; i < extensions.length; i++) {
			if(f.getName().endsWith("." + extensions[i])) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @return the extension
	 */
	public String[] getExtensions() {
		return Arrays.copyOf(extensions, extensions.length);
	}

	/* (non-Javadoc)
	 * @see javax.swing.filechooser.FileFilter#getDescription()
	 */
	@Override
	public String getDescription() {
		return description;
	}

}
