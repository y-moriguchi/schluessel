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
package net.morilib.lisp.array;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/29
 */
public class VectorToArray extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum v  = SubrUtils.nextIf(itr, mesg, body);
		Datum pr = SubrUtils.nextIf(itr, mesg, body);
		IntegerVector iv = new IntegerArrayVector();

		while(itr.hasNext()) {
			iv.add(SubrUtils.nextSmallInt(itr, mesg, body));
		}
		SubrUtils.checkTerminated(itr, body, mesg);

		if(!(v instanceof LispVector)) {
			throw mesg.getError("err.require.vector", v);
		} else if(!(pr instanceof LispArrayPrototype)) {
			throw mesg.getError("err.srfi47.require.prototype", pr);
		} else {
			ILispArray a = ((LispArrayPrototype)pr).makeArray(
					iv.toIntArray());

			try {
				a.fill(((LispVector)v).iterator());
				return (Datum)a;
			} catch(InvalidDimensionException e) {
				throw mesg.getError("err.srfi25.dimension.invalid");
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError("err.range.invalid");
			} catch(ClassCastException e) {
				throw mesg.getError("err.srfi25.typemismatch");
			} catch(ValueOutOfBoundsException e) {
				throw mesg.getError("err.srfi47.valueoutofrange",
						e.getMessage());
			}
		}
	}

}
