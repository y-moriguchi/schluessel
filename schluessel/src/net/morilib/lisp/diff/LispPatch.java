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
package net.morilib.lisp.diff;

import java.util.List;

import net.morilib.diff.Patch;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/11
 */
public class LispPatch extends UnaryArgs {

	//
	Patch<Datum> patch;

	/**
	 * 
	 * @param patch
	 */
	public LispPatch(Patch<Datum> patch) {
		this.patch = patch;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(c1a, mesg);
		List<Datum> r;

		try {
			r = patch.patch(l);
			return LispUtils.listToCons(r);
		} catch(RuntimeException e) {
			throw mesg.getError("err.diff.error.patch");
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<patch>");
	}

}
