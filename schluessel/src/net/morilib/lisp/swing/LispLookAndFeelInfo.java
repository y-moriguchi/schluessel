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
package net.morilib.lisp.swing;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

import javax.swing.UIManager;
import javax.swing.UIManager.LookAndFeelInfo;

import net.morilib.lisp.Datum2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/23
 */
public class LispLookAndFeelInfo extends Datum2 {

	//
	static final Map<String, LispLookAndFeelInfo> LnFS;

	//
	private String description;
	private String className;

	//
	static {
		Map<String, LispLookAndFeelInfo> lnfs =
				new HashMap<String, LispLookAndFeelInfo>();
		LispLookAndFeelInfo li;

		for(LookAndFeelInfo l : UIManager.getInstalledLookAndFeels()) {
			li = new LispLookAndFeelInfo(
					l.getName(), l.getClassName());
			lnfs.put(l.getName(), li);
		}
		LnFS = Collections.unmodifiableMap(lnfs);
	}

	//
	private LispLookAndFeelInfo(String desc, String className) {
		this.description = desc;
		this.className   = className;
	}

	/**
	 * 
	 * @return
	 */
	public String getDescription() {
		return description;
	}

	/**
	 * 
	 * @return
	 */
	public String getClassName() {
		return className;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<look-and-feel-class ")
		.append(description).append(">");
	}

}
