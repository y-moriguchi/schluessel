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
package net.morilib.lang.system;

import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/24
 */
public final class Version
implements Comparable<Version>, java.io.Serializable {

	/**
	 * 
	 */
	public static final Version OF_JAVA =
		new Version(System.getProperty("java.version"));

	//
	private String verstr;
	private int[] ver;

	/**
	 * 
	 * @param ver
	 * @throws NumberFormatException
	 */
	public Version(String version) {
		String[] vers;

		verstr = version;
		vers = version.split("[._\\- ]");
		ver = new int[vers.length];
		for(int i = 0; i < ver.length; i++) {
			ver[i] = Integer.parseInt(vers[i]);
		}
	}

	/**
	 * 
	 * @param vernum
	 */
	public Version(int... vernum) {
		StringBuilder b = new StringBuilder();
		String dlm = "";

		ver = new int[vernum.length];
		System.arraycopy(vernum, 0, ver, 0, ver.length);
		for(int i = 0; i < ver.length; i++) {
			b.append(dlm).append(ver[i]);
			dlm = ".";
		}
		verstr = b.toString();
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public boolean isVersionOf(Version v) {
		if(ver.length < v.ver.length) {
			return false;
		} else {
			for(int i = 0; i < v.ver.length; i++) {
				if(ver[i] != v.ver[i]) {
					return false;
				}
			}
			return true;
		}
	}

	/**
	 * 
	 * @return
	 */
	public int[] toIntArray() {
		int[] r = new int[ver.length];

		System.arraycopy(ver, 0, r, 0, r.length);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return Arrays.hashCode(ver);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof Version) {
			return Arrays.equals(ver, ((Version)o).ver);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Comparable#compareTo(java.lang.Object)
	 */
	public int compareTo(Version o) {
		for(int i = 0; i < ver.length && i < o.ver.length; i++) {
			if(ver[i] < o.ver[i]) {
				return -1;
			} else if(ver[i] > o.ver[i]) {
				return 1;
			}
		}
		return (ver.length < o.ver.length) ?
				-1 : (ver.length > o.ver.length) ? 1 : 0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return verstr;
	}

}
