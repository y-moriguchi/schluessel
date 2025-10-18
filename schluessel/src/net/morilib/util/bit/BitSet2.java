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

import java.math.BigInteger;
import java.util.ConcurrentModificationException;
import java.util.NoSuchElementException;

import net.morilib.lang.Hashes;
import net.morilib.lang.algebra.BooleanElement;
import net.morilib.util.BitUtils;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public class BitSet2
implements BooleanElement<BitSet2>, java.io.Serializable {

	//
	private static final long serialVersionUID = -829198820340136241L;

	//
	private byte[] vector;
	private BitSet2Class cls;
	private transient int modCount = 0;

	//
	/*package*/ BitSet2(BitSet2Class cls) {
		this.cls = cls;
		vector = new byte[(cls.getSize() + 7) >> 3];
	}

	/**
	 * 
	 * @param s
	 */
	public BitSet2(BitSet2 s) {
		if(s == null) {
			throw new NullPointerException();
		}
		this.cls = s.cls;
		this.vector = new byte[s.vector.length];
		System.arraycopy(
				s.vector, 0, this.vector, 0, s.vector.length);
	}

	//
	private int getBitOffset() {
		return -cls.getMinimum();
	}

	//
	private void checkRange(int v) {
		if(v < cls.getMinimum() || v > cls.getMaximum()) {
			throw new IllegalArgumentException();
		}
	}

	//
	private void checkSet(BitSet2 x) {
		if(x == null) {
			throw new NullPointerException();
		} else if(!cls.equals(x.cls)) {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#add(byte)
	 */
	public boolean add(int v) {
		int p = v + getBitOffset();
		int a = p / 8;
		int b = p & 0x7;
		int r;

		modCount++;
		checkRange(v);
		r = vector[a];
		vector[a] |= 1 << b;
		return (r & (1 << b)) == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAll(BitSet2 t) {
		boolean r = false;

		checkSet(t);
		modCount++;
		for(int i = 0; i < vector.length; i++) {
			r = (vector[i] != t.vector[i]) || r;
			vector[i] |= t.vector[i];
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#clear()
	 */
	public void clear() {
		modCount++;
		for(int i = 0; i < vector.length; i++) {
			vector[i] = 0;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#contains(byte)
	 */
	public boolean contains(int v) {
		int p = v + getBitOffset();
		int a = p / 8;
		int b = p & 0x7;

		checkRange(v);
		return (vector[a] & (1 << b)) != 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#containsAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean containsAll(BitSet2 t) {
		checkSet(t);
		for(int i = 0; i < vector.length; i++) {
			if((t.vector[i] & ~vector[i]) != 0) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isEmpty()
	 */
	public boolean isEmpty() {
		for(int i = 0; i < vector.length; i++) {
			if(vector[i] != 0) {
				return false;
			}
		}
		return true;
	}

	//
	private class Itr implements IntegerIterator {

		//
		private int ptr = -1;
		private int vec = 0;
		private int prr = -1;
		private int exModCount = modCount;

		//
		private Itr() {
			seeknext();
		}

		private void seeknext() {
			if(vec == 0) {
				while(++ptr < vector.length) {
					if(vector[ptr] != 0) {
						vec = vector[ptr];
						return;
					}
				}
				ptr = -1;
			}
		}

		public boolean hasNext() {
			return ptr >= 0;
		}

		public int next() {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			}
			prr  = BitUtils.getLsb(vec) - 1;
			vec &= ~(1 << prr);
			seeknext();
			return prr + getBitOffset();
		}

		public void remove() {
			if(exModCount != modCount) {
				throw new ConcurrentModificationException();
			} else if(prr < 0) {
				throw new IllegalStateException();
			}
			BitSet2.this.remove(prr + getBitOffset());
			prr = -1;
			exModCount = modCount;
		}

	}
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#iterator()
	 */
	public IntegerIterator iterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#remove(byte)
	 */
	public boolean remove(int v) {
		int p = v + getBitOffset();
		int a = p / 8;
		int b = p & 0x7;
		boolean r = (vector[a] & (1 << b)) != 0;

		modCount++;
		checkRange(v);
		vector[a] &= ~(1 << b);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#removeAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean removeAll(BitSet2 t) {
		boolean r = false;

		checkSet(t);
		modCount++;
		for(int i = 0; i < vector.length; i++) {
			r = ((vector[i] & t.vector[i]) != 0) || r;
			vector[i] &= ~t.vector[i];
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#retainAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean retainAll(BitSet2 t) {
		boolean r = false;

		checkSet(t);
		modCount++;
		for(int i = 0; i < vector.length; i++) {
			r = ((vector[i] & ~t.vector[i]) != 0) || r;
			vector[i] &= t.vector[i];
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#size()
	 */
	public int size() {
		int r = 0;

		for(int i = 0; i < vector.length; i++) {
			r += BitUtils.countBit(vector[i]);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#first()
	 */
	public int first() {
		for(int i = 0; i < vector.length; i++) {
			if(vector[i] != 0) {
				return (BitUtils.getLsb(vector[i]) + i * 8 -
						getBitOffset());
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#last()
	 */
	public int last() {
		for(int i = vector.length - 1; i >= 0; i++) {
			if(vector[i] != 0) {
				return (BitUtils.getMsb(vector[i]) + i * 8 -
						getBitOffset());
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#join(net.morilib.lang.algebra.BooleanElement)
	 */
	public BitSet2 join(BitSet2 x) {
		BitSet2 r;

		checkSet(x);
		r = new BitSet2(this);
		r.addAll(x);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#meet(net.morilib.lang.algebra.BooleanElement)
	 */
	public BitSet2 meet(BitSet2 x) {
		BitSet2 r;

		checkSet(x);
		r = new BitSet2(this);
		r.retainAll(x);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#complement()
	 */
	public BitSet2 complement() {
		BitSet2 r = new BitSet2(this);
		int j = 0;

		for(int i = r.cls.getSize() - 1; i > 0; i -= 8) {
			int msk = (i < 8) ? BitUtils.getIntMask(i - 1) : 0xff;

			r.vector[j] = (byte)((int)r.vector[j] ^ msk);
			j++;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is0()
	 */
	public boolean is0() {
		return isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.BooleanElement#is1()
	 */
	public boolean is1() {
		return equals(cls.get1());
	}

	/**
	 * 
	 * @return
	 */
	public BigInteger toBigInteger() {
		return new BigInteger(vector);
	}

	/**
	 * 
	 * @param t
	 * @param mask
	 * @return
	 */
	public boolean containsAllAnd(BitSet2 t, BitSet2 mask) {
		checkSet(t);
		checkSet(mask);
		for(int i = 0; i < vector.length; i++) {
			if(((t.vector[i] & mask.vector[i]) & ~vector[i]) != 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param t
	 * @param mask
	 * @return
	 */
	public boolean containsAllOr(BitSet2 t, BitSet2 mask) {
		checkSet(t);
		checkSet(mask);
		for(int i = 0; i < vector.length; i++) {
			if(((t.vector[i] | mask.vector[i]) & ~vector[i]) != 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param t
	 * @param mask
	 * @return
	 */
	public boolean equalsAnd(BitSet2 t, BitSet2 mask) {
		checkSet(t);
		checkSet(mask);
		for(int i = 0; i < vector.length; i++) {
			if((t.vector[i] & mask.vector[i]) != vector[i]) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param t
	 * @param mask
	 * @return
	 */
	public boolean equalsOr(BitSet2 t, BitSet2 mask) {
		checkSet(t);
		checkSet(mask);
		for(int i = 0; i < vector.length; i++) {
			if((t.vector[i] | mask.vector[i]) != vector[i]) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + Hashes.sumHashCode(vector));
		r = Hashes.A * (r + cls.hashCode());
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof BitSet2) {
			BitSet2 s = (BitSet2)obj;

			for(int i = 0; i < vector.length; i++) {
				if(vector[i] != s.vector[i]) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		IntegerIterator  i = iterator();
		String s = "";

		b.append("[");
		while(i.hasNext()) {
			b.append(s).append(i.next());
			s = ", ";
		}
		b.append("]");
		return b.toString();
	}

}
