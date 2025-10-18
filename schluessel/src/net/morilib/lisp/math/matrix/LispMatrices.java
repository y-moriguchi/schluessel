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
package net.morilib.lisp.math.matrix;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.matrix.ILispDatumMatrix;
import net.morilib.lisp.matrix.LispDatumMatrix;
import net.morilib.math.combinations.CombinationUtils;
import net.morilib.util.Strings;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/19
 */
public final class LispMatrices {

	//
	private LispMatrices() {}

	/**
	 * 
	 * @param matrix
	 */
	public static void print(StringBuilder b,
			ILispDatumMatrix matrix) {
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
	public static ILispMatrix toList(Datum x, LispMessage mesg) {
		ConsIterator i = new ConsIterator(x);
		ConsIterator j;
		int cls = -1, cl0 = 0, rws = 0;
		LispMatrix m;
		Datum jn;

		while(i.hasNext()) {
			j = new ConsIterator(i.next());
			while(j.hasNext()) {
				jn = j.next();
				if(!(jn instanceof LispNumber)) {
					throw mesg.getError("err.require.number", jn);
				}
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
		m = new LispMatrix(rws, cls);
		setMatrix(m, x);
		return m;
	}

	//
	private static void setMatrix(LispMatrix m, Datum x) {
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
				m.set(i, j, new Cons(r.get(i), c.get(j)));
			}
		}
		return m;
	}

	/**
	 * 
	 * @param a
	 * @return
	 * @throws LispMatrixException
	 */
	public static AbstractImmutableLispMatrix[] decomposeLU(
			ILispMatrix a) throws LispMatrixException {
		LispMatrix r = new LispMatrix(a);
		int[] order, order1;
		int   parity = 0;

		if(a.rowSize() != a.columnSize()) {
			throw new LispMatrixException();
		}

		order = new int[a.rowSize()];
		for(int i = 0; i < a.rowSize(); i++) {
			order[i] = i;
		}

		for(int i = 0; i < a.rowSize() - 1; i++) {
			int d = i, swp;
			LispReal mx = r.get(i, i).norm();
			LispNumber swp2;

			for(int j = i + 1; j < a.columnSize(); j++) {
				if(mx.compareTo(r.get(j, i).norm()) < 0) {
					d = j;
					mx = r.get(j, i).norm();
				}
			}

			if(d != i) {
				swp = order[i];
				order[i] = order[d];
				order[d] = swp;
				parity++;
				for(int j = 0; j < a.columnSize(); j++) {
					swp2 = r.get(i, j);
					r.set(i, j, r.get(d, j));
					r.set(d, j, swp2);
				}
			}

			if(r.get(i, i).norm().signum() == 0) {
				continue;
			} else {
				for(int j = i + 1; j < a.columnSize(); j++) {
					LispNumber x = r.get(j, i);

					x = x.div(r.get(i, i));
					r.set(j, i, x);
					for(int k = i + 1; k < a.rowSize(); k++) {
						LispNumber y = r.get(j, k);

						y = y.sub(x.mul(r.get(i, k)));
						r.set(j, k, y);
					}
				}
			}
		}

		order1 = new int[order.length];
		for(int i = 0; i < order.length; i++) {
			order1[order[i]] = i;
		}

		return new AbstractImmutableLispMatrix[] {
				new LispPermutationMatrix(order1, parity),
				new LUDecomposedLMatrix(r),
				new LUDecomposedUMatrix(r)
		};
	}

	//
	private static class Umd extends AbstractImmutableLispMatrix {

		//
		private ILispMatrix a;

		//
		private Umd(ILispMatrix a) {
			this.a = a;
		}

		public LispNumber get(int row, int column) {
			return a.get(row, column);
		}

		public int rowSize() {
			return a.rowSize();
		}

		public int columnSize() {
			return a.columnSize();
		}

		public LispNumber determinant() throws LispMatrixException {
			return a.determinant();
		}

		public ILispMatrix inv() throws LispMatrixException {
			return a.inv();
		}

	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public static ILispMatrix unmodifiable(ILispMatrix x) {
		return (x instanceof Umd) ? x : new Umd(x);
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @return
	 * @throws LispMatrixException
	 */
	public static ILispNumberVector solve(ILispMatrix a,
			ILispNumberVector b) throws LispMatrixException {
		ILispMatrix[] dec;
		ILispNumberVector x;

		if(a.rowSize() != a.columnSize()) {
			throw new LispMatrixException(
					"err.matrix.require.squarematrix");
		} else if(a.rowSize() != b.size()) {
			throw new LispMatrixException(
					"err.matrix.require.samesize");
		}

		// forward
		dec = decomposeLU(a);
		x   = dec[0].inv().mul(b);
		for(int i = 1; i < a.rowSize(); i++) {
			for(int j = 0; j < i; j++) {
				x.set(i, x.get(i).sub(x.get(j).mul(dec[1].get(i, j))));
			}
		}

		// backward
		for(int i = a.rowSize() - 1; i >= 0; i--) {
			if(dec[2].get(i, i).isZero()) {
				throw new LispMatrixException(
						"err.matrix.require.regular");
			} else {
				x.set(i, x.get(i).div(dec[2].get(i, i)));
				for(int j = i - 1; j >= 0; j--) {
					x.set(j, x.get(j).sub(
							x.get(i).mul(dec[2].get(j, i))));
				}
			}
		}
		return x;
	}

	/**
	 * 
	 * @param a
	 * @param order
	 * @return
	 */
	public static LispNumber determinantAllMinor(final ILispMatrix a,
			int order) {
		final int[][] cmb = new int[1][];
		LispNumber res = LispInteger.ZERO;
		int len = Math.max(a.rowSize(), a.columnSize());
		AbstractImmutableLispMatrix
		minor = new AbstractImmutableLispMatrix() {

			public LispNumber get(int row, int column) {
				return a.get(cmb[0][row], cmb[0][column]);
			}

			public int rowSize() {
				return cmb[0].length;
			}

			public int columnSize() {
				return cmb[0].length;
			}

			public LispNumber determinant(
					) throws LispMatrixException {
				ILispMatrix[] decomposed;

				if(columnSize() != rowSize()) {
					throw new LispMatrixException();
				} else {
					decomposed = LispMatrices.decomposeLU(this);
					// det(P) * det(U)
					return decomposed[0].determinant().mul(
							decomposed[2].determinant());
				}
			}

		};

		cmb[0] = CombinationUtils.initCombination(len, order);
		do {
			res = res.add(minor.determinant());
		} while((cmb[0] = CombinationUtils.nextCombination(
				len, cmb[0])) != null);
		return res;
	}

}
