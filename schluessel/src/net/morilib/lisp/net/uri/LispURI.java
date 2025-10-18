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
package net.morilib.lisp.net.uri;

import java.net.URI;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.JavaObjective;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public class LispURI extends Datum2 implements JavaObjective {

	//
	URI uri;

	/**
	 * 
	 * @param uri
	 */
	public LispURI(URI uri) {
		if(uri == null)  throw new NullPointerException();
		this.uri = uri;
	}

	/**
	 * @return the uri
	 */
	public URI getUri() {
		return uri;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	@Override
	public URI toObject() {
		return uri;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return uri.hashCode();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispURI) {
			return uri.equals(((LispURI)o).uri);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<uri ").append(uri).append(">");
	}

}
