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
package net.morilib.net.ftp;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/14
 */
public enum FTPMode {

	//
	BINARY("I", "binary"), ASCII("A", "ascii");

	//
	private String type, name;

	//
	private FTPMode(String type, String name) {
		this.type = type;
		this.name = name;
	}

	/**
	 * 
	 * @param s
	 * @return
	 */
	public static FTPMode getInstance(String s) {
		if("binary".equalsIgnoreCase(s)) {
			return BINARY;
		} else if("ascii".equalsIgnoreCase(s)) {
			return ASCII;
		} else {
			return null;
		}
	}

	/**
	 * 
	 * @return
	 */
	public String getType() {
		return type;
	}

	/* (non-Javadoc)
	 * @see java.lang.Enum#toString()
	 */
	public String toString() {
		return name;
	}

}
