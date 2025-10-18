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
package net.morilib.util.bit;

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.BooleanAlgebra;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class BitSet2Class
implements BooleanAlgebra<BitSet2>, java.io.Serializable {

	//
	private static final long serialVersionUID = 6360034344318517359L;

	//
	private int max, min;
	private transient BitSet2 zero, one;

	/**
	 * 
	 * @param min
	 * @param max
	 */
	public BitSet2Class(int min, int max) {
		if(min > max) {
			throw new IllegalArgumentException();
		}
		this.max  = max;
		this.min  = min;
		this.zero = new BitSet2(this);
		this.one  = zero.complement();
	}

	/**
	 * @return
	 */
	public int getSize() {
		return max - min + 1;
	}

	/**
	 * @return
	 */
	public int getMaximum() {
		return max;
	}

	/**
	 * @return
	 */
	public int getMinimum() {
		return min;
	}

	/**
	 * @return
	 */
	public BitSet2 newInstance() {
		return new BitSet2(this);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#get0()
	 */
	public BitSet2 get0() {
		return zero;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#get1()
	 */
	public BitSet2 get1() {
		return one;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#join(net.morilib.lang.algebra.BooleanElement, net.morilib.lang.algebra.BooleanElement)
	 */
	public BitSet2 join(BitSet2 x, BitSet2 y) {
		return x.join(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#join(A[])
	 */
	public BitSet2 join(BitSet2... as) {
		BitSet2 r = new BitSet2(get0());

		for(BitSet2 a : as) {
			r.addAll(a);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#meet(net.morilib.lang.algebra.BooleanElement, net.morilib.lang.algebra.BooleanElement)
	 */
	public BitSet2 meet(BitSet2 x, BitSet2 y) {
		return x.meet(y);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#meet(A[])
	 */
	public BitSet2 meet(BitSet2... as) {
		BitSet2 r = new BitSet2(get1());

		for(BitSet2 a : as) {
			r.retainAll(a);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanAlgebra#complement(net.morilib.lang.algebra.BooleanElement)
	 */
	public BitSet2 complement(BitSet2 x) {
		return x.complement();
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + max);
		r = Hashes.A * (r + min);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof BitSet2Class) {
			BitSet2Class c = (BitSet2Class)obj;

			return (max == c.max) && (min == c.min);
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "BitSetClass:[" + min + ", " + max + "]";
	}

	//
	private void readObject(
			java.io.ObjectInputStream s
			) throws java.io.IOException, ClassNotFoundException {
		s.defaultReadObject();
		this.zero = new BitSet2(this);
		this.one  = zero.complement();
	}

}
