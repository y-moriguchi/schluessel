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

import java.util.Arrays;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.util.primitive.IntegerArrayVector;
import net.morilib.util.primitive.IntegerVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/28
 */
public class ListToArray extends TernaryArgs {

	//
	private static void setListToArray(ILispArray a, Datum d, int i,
			int[] is) {
		if(i < a.rank()) {
			ConsIterator itr;

			if(!(d instanceof Cons)) {
				throw new InvalidDimensionException();
			} else {
				itr = new ConsIterator(d);
				while(itr.hasNext()) {
					setListToArray(a, itr.next(), i + 1, is);
					is[i]++;
				}
				is[i] = 0;
			}
		} else {
			a.setToArray(d, is);
		}
	}

	//
	private static void getdimensions(Datum p, Datum d, int dp,
			IntegerVector iz, LispMessage mesg) {
		if(d instanceof Cons) {
			int l = (int)LispUtils.length(d);
			ConsIterator itr;

			if(l < 0) {
				throw mesg.getError("err.list.circulated", p);
			} else if(dp >= iz.size()) {
				iz.add(l);
			} else if(iz.getInt(dp) != l) {
				throw mesg.getError("err.srfi58.list.invalid", p);
			}

			itr = new ConsIterator(d);
			while(itr.hasNext()) {
				getdimensions(p, itr.next(), dp + 1, iz, mesg);
			}
		}
	}

	//
	/*package*/ static int[] getDimensions(Datum p, LispMessage mesg) {
		IntegerVector iz = new IntegerArrayVector();

		getdimensions(p, p, 0, iz, mesg);
		return iz.toIntArray();
	}

	/**
	 * 
	 * @param a
	 * @param d
	 * @return
	 */
	public static ILispArray setListToArray(ILispArray a, Datum d) {
		int[] x = new int[a.rank()];

		Arrays.fill(x, 0);
		setListToArray(a, d, 0, x);
		return a;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		int rank = SubrUtils.getSmallInt(c1a, mesg);

		if(c2a instanceof LispArrayPrototype) {
			ILispArray a;
			int[] is = getDimensions(c3a, mesg);

			if(is.length != rank) {
				throw mesg.getError(
						"err.srfi58.notation.mismatch.rank",
						"" + rank);
			}

			try {
				a = ((LispArrayPrototype)c2a).makeArray(is);
				setListToArray(a, c3a);
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
		} else {
			throw mesg.getError("err.srfi47.require.prototype", c2a);
		}
	}

}
