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
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
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
public abstract class BytevectorToList extends Subr {

	/**
	 * 
	 * @param e
	 * @param a
	 * @param k
	 * @param s
	 * @return
	 */
	protected abstract Datum getInt(
			Endianness2 e, byte[] a, int k, int s);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsListBuilder b = new ConsListBuilder();
		ConsIterator itr = new ConsIterator(body);
		LispBytevector v = LispBytevector.datumToBytevector(
				SubrUtils.nextIf(itr, mesg, body), mesg);
		Datum d3 = SubrUtils.nextIf(itr, mesg, body);
		Datum d4 = Iterators.nextIf(itr);
		Datum d5 = Iterators.nextIf(itr);
		Datum d6 = Iterators.nextIf(itr);
		Endianness2 e;
		int s, f, t;

		if(d3 == null || d3 instanceof LispNumber) {
			e = SynEndianness.getNative();
			s = SubrUtils.getSmallInt(d3, mesg);
			d6 = d5;  d5 = d4;
		} else if(d3 instanceof Symbol) {
			e = SynEndianness.getEndianness(d3, mesg);
			s = SubrUtils.getSmallInt(d4, mesg);
		} else {
			throw mesg.getError("err.r6rs.require.endianness", d3);
		}

		f = (d5 == null) ? 0 : SubrUtils.getSmallInt(d5, mesg);
		t = (d6 == null) ?
				v.length() : SubrUtils.getSmallInt(d6, mesg);
		if(s <= 0) {
			throw mesg.getError("err.require.int.positive", s);
		} else if(f < 0 || f >= v.length()) {
			throw mesg.getError("err.range.invalid", f);
		} else if(t < 0 || t > v.length()) {
			throw mesg.getError("err.range.invalid", t);
		} else if(t < f) {
			throw mesg.getError("err.range.invalid");
		} else if((t - f) % s != 0) {
			throw mesg.getError("err.r6rs.range.mustbemultiple");
		}

		for(int k = f; k < t; k += s) {
			b.append(getInt(e, v.vector, k, s));
		}
		return b.get();
	}

}
