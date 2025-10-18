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
import net.morilib.util.BitUtils;
import net.morilib.util.primitive.IntegerArrayVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/26
 */
public class PackedBitVector extends AbstractBitVector
implements java.io.Serializable {

	//
	private static final long serialVersionUID = -8401650979234951542L;

	//
	/*package*/ IntegerArrayVector vector;
	/*package*/ int size;

	/**
	 * 
	 */
	public PackedBitVector() {
		vector = new IntegerArrayVector();
		size   = 0;
	}

	/**
	 * 
	 * @param mask
	 */
	public PackedBitVector(int mask) {
		vector = new IntegerArrayVector();
		size   = 32;
		vector.add(mask);
	}

	/**
	 * 
	 * @param vec
	 */
	public PackedBitVector(PackedBitVector vec) {
		vector = new IntegerArrayVector(vec.vector);
		size   = vec.size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#getBoolean(int)
	 */
	public boolean getBoolean(int index) {
		int i = index >> 5;
		int y = index & 0x1f;

		if(index < 0 || index >= size) {
			throw new IndexOutOfBoundsException("" + index);
		}
		return (vector.getInt(i) & (1 << y)) != 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#size()
	 */
	public int size() {
		return size;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#add(boolean)
	 */
	@Override
	public boolean add(boolean x) {
		int i = size >> 5;
		int y = size & 0x1f;
		int a;

		if(y == 0) {
			vector.add(0);
			a = 0;
		} else {
			a = vector.getInt(i);
		}
		a = x ? a | (1 << y) : a & ~(1 << y);
		vector.set(i, a);
		size++;
		modcount++;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#clear()
	 */
	@Override
	public void clear() {
		vector.clear();
		size = 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#any(boolean)
	 */
	@Override
	public boolean any(boolean x) {
		int i, g, s, z;

		s = size & 0x1f;
		z = (s > 0) ? 1 : 0;
		for(i = 0; i < vector.size() - z; i++) {
			g = vector.getInt(i);
			if((x && g != 0) || (!x && g != 0xffffffff)) {
				return true;
			}
		}

		if(s > 0) {
			g = vector.get(i);
			return (( x && g != 0) ||
					(!x && g != BitUtils.getIntMask(s - 1)));
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#every(boolean)
	 */
	@Override
	public boolean every(boolean x) {
		int i, g, s, z;

		s = size & 0x1f;
		z = (s > 0) ? 1 : 0;
		for(i = 0; i < vector.size() - z; i++) {
			g = vector.getInt(i);
			if((!x && g != 0) || (x && g != 0xffffffff)) {
				return false;
			}
		}

		if(s > 0) {
			g = vector.get(i);
			return ((!x && g == 0) ||
					( x && g == BitUtils.getIntMask(s - 1)));
		} else {
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#add(int, boolean)
	 */
	@Override
	public boolean add(int index, boolean x) {
		int i = index >> 5;
		int y = index & 0x1f;
		int z;
		boolean b;

		if(index < 0 || index > size) {
			throw new IndexOutOfBoundsException("" + index);
		}

		// shift
		if((size & 0x1f) == 0) {
			vector.add(0);
		}
		z = vector.getInt(i);
		b = (z & 0x80000000) != 0;
		if(y > 0) {
			z = ((z & BitUtils.getIntMask(y - 1)) |
					((z << 1) & ~BitUtils.getIntMask(y - 1)));
		} else {
			z = z << 1;
		}
		vector.set(i, z);
		for(int j = i + 1; j <= (size >> 5); j++) {
			z = vector.getInt(j);
			vector.set(j, (z << 1) | (b ? 1 : 0));
			b = (z & 0x80000000) != 0;
		}
		size++;

		set(index, x);
		modcount++;
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#removeAt(int)
	 */
	@Override
	public boolean removeAt(int index) {
		int i = index >> 5;
		int y = index & 0x1f;
		int z;
		boolean b, r;

		if(index < 0 || index >= size) {
			throw new IndexOutOfBoundsException("" + index);
		}
		r = getBoolean(index);

		// shift
		b = false;
		for(int j = ((size - 1) >> 5); j > i; j--) {
			z = vector.getInt(j);
			vector.set(j, (z >>> 1) | (b ? 0x80000000 : 0));
			b = (z & 1) != 0;
		}
		z = vector.getInt(i);
		if(y > 0) {
			z = ((z & BitUtils.getIntMask(y - 1)) |
					((z >>> 1) & ~BitUtils.getIntMask(y - 1)) |
					(b ? 0x80000000 : 0));
		} else {
			z = (z >>> 1) | (b ? 0x80000000 : 0);
		}
		vector.set(i, z);
		if((size & 0x1f) == 1) {
			vector.remove(vector.size() - 1);
		}
		size--;

		modcount++;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#set(int, boolean)
	 */
	@Override
	public boolean set(int index, boolean x) {
		int i = index >> 5;
		int y = index & 0x1f;
		int a;
		boolean r;

		if(index < 0 || index >= size) {
			throw new IndexOutOfBoundsException("" + index);
		}
		r = getBoolean(index);
		a = vector.getInt(i);
		a = x ? a | (1 << y) : a & ~(1 << y);
		vector.set(i, a);
		modcount++;
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#negate()
	 */
	@Override
	public void negate() {
		int i, s, z;

		s = size & 0x1f;
		z = (s > 0) ? 1 : 0;
		for(i = 0; i < vector.size() - z; i++) {
			vector.set(i, ~vector.getInt(i));
		}

		if(s > 0) {
			vector.set(i, vector.get(i) ^ BitUtils.getIntMask(s - 1));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof PackedBitVector) {
			PackedBitVector v = (PackedBitVector)o;

			return vector.equals(v.vector) && size == v.size;
		}
		return super.equals(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.AbstractBitVector#hashCode()
	 */
	@Override
	public int hashCode() {
		if(size == 0) {
			return 0;
		} else {
			return (Hashes.INIT * vector.get(0) + size()) * Hashes.A;
		}
	}

}
