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
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/22
 */
public class ArraySetS extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env,
			LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum a = SubrUtils.nextIf(itr, mesg, body);
		Datum d = SubrUtils.nextIf(itr, mesg, body);
		int[] i;

		if(!(a instanceof ILispArray)) {
			throw mesg.getError("err.srfi25.require.array", a);
		} else if(d instanceof LispArrayIndices) {
			i = ((LispArrayIndices)d).getpos();
		} else if(d instanceof ILispArray) {
			ILispArray b = (ILispArray)d;
			LispVector v;

			if(b.rank() != 1) {
				throw mesg.getError("err.srfi25.dimension.invalid",
						d);
			}

			v = b.toVector();
			i = new int[v.size()];
			for(int j = 0; j < v.size(); j++) {
				i[j] = SubrUtils.getSmallInt(v.get(j),
						mesg);
			}
			d = SubrUtils.nextIf(itr, mesg, body);
		} else {
			IntegerVector iv = new IntegerArrayVector();

			do {
				if(!itr.hasNext()) {
					break;
				}
				iv.add(SubrUtils.getSmallInt(d, mesg));
			} while((d = Iterators.nextIf(itr)) != null);
			i = iv.toIntArray();
		}

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			((ILispArray)a).setToArray(d, i);
			return Undef.UNDEF;
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