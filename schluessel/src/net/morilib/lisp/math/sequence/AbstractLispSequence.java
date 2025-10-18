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
package net.morilib.lisp.math.sequence;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.iterator.ILispIterable;
import net.morilib.lisp.iterator.ILispIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/11
 */
public abstract class AbstractLispSequence extends Datum2
implements ILispSequence, ILispIterable {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispAddable#add(net.morilib.lisp.math.algebra.ILispAddable)
	 */
	public ILispSequence add(final ILispSequence y) {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite() && y.isFinite();
			}

			public int size() {
				return isFinite() ? Math.max(t.size(), y.size()) : -1;
			}

			public LispNumber get(int i) {
				return t.get(i).add(y.get(i));
			}

			public LispNumber limit() {
				return t.limit().add(y.limit());
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispSubtractable#sub(net.morilib.lisp.math.algebra.ILispSubtractable)
	 */
	public ILispSequence sub(final ILispSequence y) {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite() && y.isFinite();
			}

			public int size() {
				return isFinite() ? Math.max(t.size(), y.size()) : -1;
			}

			public LispNumber get(int i) {
				return t.get(i).sub(y.get(i));
			}

			public LispNumber limit() {
				return t.limit().sub(y.limit());
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispNegatable#uminus()
	 */
	public ILispSequence uminus() {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite();
			}

			public int size() {
				return t.size();
			}

			public LispNumber get(int i) {
				return t.get(i).uminus();
			}

			public LispNumber limit() {
				return t.limit().uminus();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispMultipliable#mul(net.morilib.lisp.math.algebra.ILispMultipliable)
	 */
	public ILispSequence mul(final ILispSequence y) {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite() && y.isFinite();
			}

			public int size() {
				return isFinite() ? Math.max(t.size(), y.size()) : -1;
			}

			public LispNumber get(int i) {
				return t.get(i).mul(y.get(i));
			}

			public LispNumber limit() {
				return t.limit().mul(y.limit());
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispDividable#div(net.morilib.lisp.math.algebra.ILispDividable)
	 */
	public ILispSequence div(final ILispSequence y) {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite() && y.isFinite();
			}

			public int size() {
				return isFinite() ? Math.max(t.size(), y.size()) : -1;
			}

			public LispNumber get(int i) {
				return t.get(i).div(y.get(i));
			}

			public LispNumber limit() {
				return t.limit().div(y.limit());
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.algebra.ILispInvertable#inv()
	 */
	public ILispSequence inv() {
		final ILispSequence t = this;

		return new AbstractLispSequence() {

			public boolean isFinite() {
				return t.isFinite();
			}

			public int size() {
				return t.size();
			}

			public LispNumber get(int i) {
				return t.get(i).inv();
			}

			public LispNumber limit() {
				return t.limit().inv();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.iterator.ILispIterable#lispIterator()
	 */
	public ILispIterator lispIterator() {
		final int[] c = new int[1];

		c[0] = 1;
		return new ILispIterator() {

			public boolean isTerminated() {
				return c[0] >= size() || c[0] <= 0;
			}

			public ILispIterator next() {
				c[0]++;
				return this;
			}

			public Datum getCurrentDatum() {
				return get(c[0]);
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		int i;

		buf.append("{");
		for(i = 0; i < 10 && i < size(); i++) {
			buf.append(" ").append(get(i));
		}
		if(i < size())  buf.append("...");
		buf.append("}");
	}

}
