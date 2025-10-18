/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class JavaNull extends Datum {

	/**
	 * 
	 */
	public static final JavaNull JAVA_NULL = new JavaNull();

	//
	private JavaNull() {
		// do nothing
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toString()
	 */
	public String toString() {
		return "null";
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<java-null>");
	}

}
