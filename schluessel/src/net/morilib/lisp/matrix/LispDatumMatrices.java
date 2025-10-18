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
package net.morilib.lisp.matrix;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.matrix.ILispDatumMatrix;
import net.morilib.lisp.matrix.LispDatumMatrix;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public final class LispDatumMatrices {

	//
	private LispDatumMatrices() {}

	/**
	 * 
	 * @param matrix
	 */
	public static void print(StringBuilder b, ILispDatumMatrix matrix) {
		int l = -1;
		String dlm = "";

		for(int i = 0; i < matrix.rowSize(); i++) {
			for(int j = 0; j < matrix.columnSize(); j++) {
				Datum  d = matrix.get(i, j);
				String s = LispUtils.print(d);

				l = (l < s.length()) ? s.length() : l;
			}
		}

		for(int i = 0; i < matrix.rowSize(); i++) {
			b.append("[");
			dlm = "";
			for(int j = 0; j < matrix.columnSize(); j++) {
				Datum  d = matrix.get(i, j);
				String s = LispUtils.print(d);

				b.append(dlm).append(Strings.rpad(s, l));
				dlm = " ";
			}
			b.append("]\n");
		}
	}

	/**
	 * 
	 * @param x
	 * @param mesg
	 * @return
	 */
	public static ILispDatumMatrix toDatumList(Datum x, LispMessage mesg) {
		ConsIterator i = new ConsIterator(x);
		ConsIterator j;
		int cls = -1, cl0 = 0, rws = 0;
		LispDatumMatrix m;

		while(i.hasNext()) {
			j = new ConsIterator(i.next());
			while(j.hasNext()) {
				j.next();
				cl0++;
			}

			if(cls < 0) {
				cls = cl0;
			} else if(cls != cl0) {
				throw mesg.getError(
						"err.matrix.require.samecolumnsize");
			}
			cl0 = 0;
			rws++;
		}

		if(cls <= 0 || rws <= 0) {
			throw mesg.getError("err.matrix.require.listoflist", x);
		}
		m = new LispDatumMatrix(rws, cls);
		setMatrix(m, x);
		return m;
	}

	//
	private static void setMatrix(LispDatumMatrix m, Datum x) {
		ConsIterator i = new ConsIterator(x);
		ConsIterator j;
		int cls = 0, rws = 0;

		while(i.hasNext()) {
			j = new ConsIterator(i.next());
			while(j.hasNext()) {
				m.set(rws, cls, j.next());
				cls++;
			}
			rws++;
			cls = 0;
		}
	}

	/**
	 * 
	 * @param r
	 * @param c
	 * @return
	 */
	public static LispDatumMatrix consTensorProduct(ILispVector r,
			ILispVector c) {
		LispDatumMatrix m = new LispDatumMatrix(r.size(), c.size());

		for(int i = 0; i < r.size(); i++) {
			for(int j = 0; j < c.size(); j++) {
				m.set(i, j, LispUtils.mul(r.get(i), c.get(j)));
			}
		}
		return m;
	}

}
