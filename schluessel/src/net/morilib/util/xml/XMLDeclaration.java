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
package net.morilib.util.xml;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
public class XMLDeclaration {

	//
	/*package*/ String encoding;
	/*package*/ String version;
	/*package*/ Boolean standalone;

	/**
	 * @return
	 */
	public static XMLDeclaration newDefault() {
		XMLDeclaration d = new XMLDeclaration();

		d.encoding   = null;
		d.version    = null;
		d.standalone = null;
		return d;
	}

	/**
	 * @return the encoding
	 */
	public String getEncoding() {
		return encoding;
	}

	/**
	 * @return the version
	 */
	public String getVersion() {
		return version;
	}

	/**
	 * @return the standalone
	 */
	public boolean isStandalone() {
		return standalone == null || standalone.booleanValue();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	@Override
	public String toString() {
		StringBuilder b = new StringBuilder();
		String d = "";

		if(version != null) {
			b.append("version='").append(version).append("'");
			d = " ";
		}
		if(encoding != null) {
			b.append(d);
			b.append("encoding='").append(encoding).append("'");
			d = " ";
		}
		if(standalone != null) {
			b.append(d);
			b.append("standalone='");
			b.append((standalone ? "yes" : "no"));
			b.append("'");
		}
		return b.toString();
	}

}
