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
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.math.LispOrdinalNumber;
import net.morilib.lisp.math.NoMoreOrdinalNumberException;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/23
 */
public class LispArrayIndices extends Datum2
implements LispOrdinalNumber {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/23
	 */
	public static class MakeArrayIndices extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum d = Iterators.nextIf(itr);

			if(d == null) {
				throw mesg.getError("err.argument", body);
			} else if(d instanceof ILispArray) {
				SubrUtils.checkTerminated(itr, body, mesg);
				return new LispArrayIndices(((ILispArray)d).getShape());
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	//
	private LispArrayShape shape;
	private int[] pos;

	//
	/*package*/ LispArrayIndices(LispArrayShape shape) {
		this.shape = shape;
		pos = new int[shape.rank()];
		for(int i = 0; i < pos.length; i++) {
			pos[i] = shape.getStartIndex(i);
		}
	}

	//
	private LispArrayIndices prototype(int[] pos) {
		LispArrayIndices i = new LispArrayIndices(shape);

		System.arraycopy(pos, 0, i.pos, 0, pos.length);
		return i;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.LispOrdinalNumber#inclement()
	 */
	public LispOrdinalNumber inclement() {
		LispArrayIndices ind = prototype(pos);

		for(int i = pos.length - 1; i >= 0; i--) {
			if(++ind.pos[i] < shape.getEndIndex(i)) {
				return ind;
			} else {
				ind.pos[i] = shape.getStartIndex(i);
			}
		}
		throw new NoMoreOrdinalNumberException();
	}

	//
	/*package*/ int[] getpos() {
		return pos;  // never overwrite!
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<array-indices>");
	}

}
