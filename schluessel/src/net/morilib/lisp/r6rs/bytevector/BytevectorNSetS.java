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
package net.morilib.lisp.r6rs.bytevector;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Endianness2;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/18
 */
public abstract class BytevectorNSetS extends Subr {

	/**
	 * 
	 * @param e
	 * @param a
	 * @param k
	 * @return
	 */
	protected abstract Datum setInt(Endianness2 e, byte[] a, int k,
			LispReal r);

	/**
	 * 
	 * @param i
	 * @param mesg
	 * @return
	 */
	protected abstract LispReal checkNumber(Datum r, LispMessage mesg);

	/**
	 * 
	 * @return
	 */
	protected abstract int getSize();

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		LispBytevector v = LispBytevector.datumToBytevector(
				SubrUtils.nextIf(itr, mesg, body), mesg);
		int k = SubrUtils.nextSmallInt(itr, mesg, body);
		Datum r  = SubrUtils.nextIf(itr, mesg, body);
		Datum d3 = Iterators.nextIf(itr);
		Endianness2 e;

		SubrUtils.checkTerminated(itr, body, mesg);
		if(d3 == null) {
			e = SynEndianness.getNative();
		} else if(d3 instanceof Symbol) {
			e = SynEndianness.getEndianness(d3, mesg);
		} else {
			throw mesg.getError("err.r6rs.require.endianness", d3);
		}

		if(k < 0 || k + getSize() > v.length()) {
			throw mesg.getError("err.range.invalid", k);
		}
		return setInt(e, v.vector, k, checkNumber(r, mesg));
	}

}
