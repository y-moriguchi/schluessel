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
package net.morilib.lisp.subr;

import net.morilib.lisp.LispMessage;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class NumLessThan extends NumCompare {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.NumCompare#compare(java.lang.Comparable, java.lang.Comparable, net.morilib.lisp.LispMessage)
	 */
	@SuppressWarnings({ "rawtypes", "unchecked" })
	@Override
	protected boolean compare(Comparable c1a, Comparable c2a,
			LispMessage mesg) {
		return ((Comparable)c1a).compareTo((Comparable)c2a) < 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#toString()
	 */
	public String toString() {
		return "Subr:<";
	}

}
