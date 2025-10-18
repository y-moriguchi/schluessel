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
package net.morilib.lisp.r6rs.enums;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.collection.enums.LispEnumSetClass;
import net.morilib.lisp.collection.enums.LispEnumSetClass.LispEnumSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/22
 */
public class EnumSetUnion extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		ILispR6RSEnum s;
		LispEnumSet r = null;

		while(itr.hasNext()) {
			s = R6RSEnumUtils.getEnum(itr.next(), mesg);
			if(r == null) {
				if(s instanceof LispEnumSetClass) {
					r = ((LispEnumSetClass)s).getUniverseEnumSet();
				} else if(s instanceof LispEnumSet) {
					r = (LispEnumSet)s;
				} else {
					throw mesg.getError("err.r6rs.require.enum",
							(Datum)s);
				}
			} else if(s instanceof LispEnumSetClass) {
				if(!s.isEqualTo(r.getEnumSetClass())) {
					throw mesg.getError(
							"err.r6rs.require.enum.sameuniverse",
							(Datum)s);
				} else {
					r = ((LispEnumSetClass)s).getUniverseEnumSet();
				}
			} else if(s instanceof LispEnumSet) {
				if(!r.getEnumSetClass().isEqualTo(
						((LispEnumSet)s).getEnumSetClass())) {
					throw mesg.getError(
							"err.r6rs.require.enum.sameuniverse",
							(Datum)s);
				} else {
					r = r.union((LispEnumSet)s);
				}
			} else {
				throw mesg.getError("err.r6rs.require.enum",
						(Datum)s);
			}
		}
		return r;
	}

}
