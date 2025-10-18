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
package net.morilib.lisp.r6rs.record;

import java.util.LinkedHashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SenaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/11
 */
public class MakeRecordTypeDescriptor extends SenaryArgs {

	//
	private static final Symbol MUTABLE   =
		Symbol.getSymbol("mutable");
	private static final Symbol IMMUTABLE =
		Symbol.getSymbol("immutable");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.SenaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Datum c5a, Datum c6a, Environment env, LispMessage mesg) {
		String nam = SubrUtils.getSymbolName(c1a, mesg);
		RecordTypeDescriptor rtd = null;
		String uid = c3a.isTrue() ?
				SubrUtils.getSymbolName(c3a, mesg) : null;
		boolean sld = c4a.isTrue();
		boolean opq = c5a.isTrue();
		Map<String, Boolean> fld =
			new LinkedHashMap<String, Boolean>();
		LispVector vec;

		if(c2a.isTrue() && !(c2a instanceof RecordTypeDescriptor)) {
			throw mesg.getError("err.r6rsrecord.require.rtd", c2a);
		} else if(!(c6a instanceof LispVector)) {
			throw mesg.getError("err.require.vector", c6a);
		}

		rtd = c2a.isTrue() ? (RecordTypeDescriptor)c2a : null;
		vec = (LispVector)c6a;
		if(rtd != null && rtd.isSealed()) {
			throw mesg.getError("err.r6rsrecord.sealed", c2a);
		} else if(rtd != null && rtd.isOpaque()) {
			opq = true;
		}

		for(int i = 0; i < vec.size(); i++) {
			Datum  x = SubrUtils.car(vec.get(i), mesg);
			String s = SubrUtils.getSymbolName(
					SubrUtils.cadr(vec.get(i), mesg), mesg);

			SubrUtils.cddrNull(vec.get(i), mesg);
			if(x.equals(MUTABLE)) {
				fld.put(s, Boolean.TRUE);
			} else if(x.equals(IMMUTABLE)) {
				fld.put(s, Boolean.FALSE);
			} else {
				throw mesg.getError("err.r6rsrecord.require.mutablity",
						vec.get(i));
			}
		}

		try {
			return LispRecordTypeDescriptor.newInstance(
					nam, rtd, uid, sld, opq, fld);
		} catch (RTDCreationException e) {
			throw mesg.getError(
					"err.r6rsrecord.assert.rtd.cannotcreate");
		}
	}

}
