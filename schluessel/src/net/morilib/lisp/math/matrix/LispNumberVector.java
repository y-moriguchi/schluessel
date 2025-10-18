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

import java.util.Arrays;
import java.util.Iterator;
import java.util.NoSuchElementException;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispComplex;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.LispQuaternion;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/23
 */
public class LispNumberVector extends Datum2
implements ILispNumberVector {

	//
	private LispNumber[] array;

	/**
	 * 
	 * @param size
	 */
	public LispNumberVector(int size) {
		this(size, LispInteger.ZERO);
	}

	/**
	 * 
	 * @param size
	 * @param fill
	 */
	public LispNumberVector(int size, LispNumber fill) {
		this.array = new LispNumber[size];
		Arrays.fill(array, fill);
	}

	//
	/*package*/ LispNumberVector(LispNumber... nums) {
		this.array = nums;
	}

	/**
	 * 
	 * @param nums
	 * @return
	 */
	public static LispNumberVector newInstance(LispNumber... nums) {
		LispNumber[] c = new LispNumber[nums.length];

		System.arraycopy(nums, 0, c, 0, nums.length);
		return new LispNumberVector(c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.ILispVector#size()
	 */
	public int size() {
		return array.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#get(int)
	 */
	public LispNumber get(int index) {
		return array[index];
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#set(int, net.morilib.lisp.LispNumber)
	 */
	public void set(int index, LispNumber x) {
		array[index] = x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#multiply(net.morilib.lisp.LispNumber)
	 */
	public ILispNumberVector mul(LispNumber x) {
		LispNumberVector r = new LispNumberVector(array.length);

		for(int i = 0; i < array.length; i++) {
			r.array[i] = array[i].mul(x);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#add(net.morilib.lisp.matrix.ILispNumberVector)
	 */
	public ILispNumberVector add(ILispNumberVector v) {
		LispNumberVector r = new LispNumberVector(array.length);

		if(array.length != v.size()) {
			throw new LispMatrixException();
		} else {
			for(int i = 0; i < array.length; i++) {
				r.array[i] = array[i].add(v.get(i));
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#subtract(net.morilib.lisp.matrix.ILispNumberVector)
	 */
	public ILispNumberVector sub(ILispNumberVector v) {
		LispNumberVector r = new LispNumberVector(array.length);

		if(array.length != v.size()) {
			throw new LispMatrixException();
		} else {
			for(int i = 0; i < array.length; i++) {
				r.array[i] = array[i].sub(v.get(i));
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#negate()
	 */
	public ILispNumberVector uminus() {
		LispNumberVector r = new LispNumberVector(array.length);

		for(int i = 0; i < array.length; i++) {
			r.array[i] = array[i].uminus();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Iterable#iterator()
	 */
	public Iterator<LispNumber> iterator() {
		return new Iterator<LispNumber>() {

			private int p = 0;

			public boolean hasNext() {
				return p < array.length;
			}

			public LispNumber next() {
				if(p >= array.length) {
					throw new NoSuchElementException();
				}
				return array[p++];
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#innerProduct(net.morilib.lisp.matrix.ILispNumberVector)
	 */
	public LispNumber innerProduct(
			ILispNumberVector b) throws LispMatrixException {
		if(b.size() != size()) {
			throw new LispMatrixException();
		} else {
			LispNumber x = LispInteger.ZERO;

			for(int i = 0; i < size(); i++) {
				x = x.add(get(i).mul(b.get(i)));
			}
			return x;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.matrix.ILispNumberVector#normSquared()
	 */
	public LispNumber normSquared() {
		LispNumber x = LispInteger.ZERO;

		for(int i = 0; i < size(); i++) {
			x = x.add(get(i).mul(get(i)));
		}
		return x;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isQuoternionVector()
	 */
	@Override
	public boolean isQuaternionVector() {
		for(int i = 0; i < size(); i++) {
			if(!(get(i) instanceof LispQuaternion)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isComplexVector()
	 */
	@Override
	public boolean isComplexVector() {
		for(int i = 0; i < size(); i++) {
			if(!(get(i) instanceof LispComplex)) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isRealVector()
	 */
	@Override
	public boolean isRealVector() {
		for(int i = 0; i < size(); i++) {
			if(!get(i).isReal()) {
				return false;
			}
		}
		return true;
	}

//	/**
//	 * 
//	 * @return
//	 */
//	public int hashCode() {
//		int r = Hashes.INIT;
//
//		for(int i = 0; i < size(); i++) {
//			r = Hashes.A * (r + get(i).hashCode());
//		}
//		return r;
//	}

//	/**
//	 * 
//	 * @param o
//	 * @return
//	 */
//	public boolean equals(Object o) {
//		if(o instanceof ILispNumberVector) {
//			ILispNumberVector y = (ILispNumberVector)o;
//
//			if(size() == y.size()) {
//				for(int i = 0; i < size(); i++) {
//					if(!get(i).isEqualTo(y.get(i))) {
//						return false;
//					}
//				}
//				return true;
//			} else {
//				return false;
//			}
//		}
//		return false;
//	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.matrix.ILispNumberVector#isEqualTo(net.morilib.lisp.math.matrix.ILispNumberVector)
	 */
	public boolean isEqualTo(ILispNumberVector y) {
		if(size() == y.size()) {
			for(int i = 0; i < size(); i++) {
				if(!get(i).isEqualTo(y.get(i))) {
					return false;
				}
			}
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("(number-vector");
		for(LispNumber x : this) {
			buf.append(" ").append(LispUtils.print(x));
		}
		buf.append(")");
	}

}
