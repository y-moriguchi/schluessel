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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispString;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public abstract class IntrospectURIStringSubr
extends IntrospectURISubr {

	/**
	 * @param c1a
	 * @return
	 */
	protected abstract String introspectString(URI c1a);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.net.uri.IntrospectURISubr#introspect(net.morilib.lisp.Datum)
	 */
	@Override
	protected Datum introspect(URI c1a) {
		String s = introspectString(c1a);

		return (s != null) ? new LispString(s) : LispBoolean.FALSE;
	}

}
