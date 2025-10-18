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
package net.morilib.lisp.sssp;

import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/30
 */
public class MakeCustomTag extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env,
			LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Map<String, Integer> attrs;
		Map<String, TagTransformer> types;
		Datum l;

		attrs = new HashMap<String, Integer>();
		types = new HashMap<String, TagTransformer>();
		l = SubrUtils.nextIf(itr, mesg, body);
		if(!(l instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", l);
		}

		for(int i = 0; itr.hasNext(); i++) {
			Datum d = itr.next(), a, t;
			TagTransformer tt;
			String as;

			if(d instanceof Cons) {
				a  = SubrUtils.cadr(d, mesg);
				t  = SubrUtils.cadr(d, mesg);
				tt = TagTransformer.getInstance(t);
				if(tt == null) {
					throw mesg.getError("err.sss.tagtype.invalid");
				}
			} else {
				a  = d;
				tt = TagTransformer.STRING;
			}
			as = SubrUtils.getSymbolName(a, mesg);
			attrs.put(as, i);
			types.put(as, tt);
		}
		SubrUtils.checkTerminated(itr, body, mesg);
		return new LispCustomTag(attrs, types, (Procedure)l);
	}

}
