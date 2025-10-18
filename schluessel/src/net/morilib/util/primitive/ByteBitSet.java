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
package net.morilib.util.primitive;

import net.morilib.util.bit.BitSet2;
import net.morilib.util.bit.BitSet2Class;
import net.morilib.util.primitive.iterator.ByteIterator;
import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public class ByteBitSet extends AbstractByteSet
implements ByteSortedSet {
	
	//
	private static final BitSet2Class BYTE_CLS =
		new BitSet2Class(-128, 127);
	
	//
	private BitSet2 bits = BYTE_CLS.newInstance();
	
	/**
	 * @param byteArrayVector
	 */
	public ByteBitSet(ByteCollection a) {
		addAllByte(a);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#add(byte)
	 */
	public boolean addByte(byte v) {
		return bits.add(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(ByteCollection a) {
		ByteSet s = a.toSet();
		
		if(s instanceof ByteBitSet) {
			return bits.addAll(((ByteBitSet)s).bits);
		} else if(s instanceof ByteCollections.UnmodifiableSet) {
			return addAllByte(
					((ByteCollections.UnmodifiableSet)s).wrapee);
		} else {
			return super.addAllByte(s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#clear()
	 */
	public void clear() {
		bits.clear();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#contains(byte)
	 */
	public boolean containsByte(byte v) {
		return bits.contains(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#containsAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean containsAllByte(ByteCollection a) {
		ByteSet s = a.toSet();
		
		if(s instanceof ByteBitSet) {
			return bits.containsAll(((ByteBitSet)s).bits);
		} else if(s instanceof ByteCollections.UnmodifiableSet) {
			return containsAllByte(
					((ByteCollections.UnmodifiableSet)s).wrapee);
		} else {
			return super.containsAllByte(s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return bits.isEmpty();
	}
	
	//
	private class Itr implements ByteIterator {
		
		//
		private IntegerIterator itr;
		
		//
		private Itr(IntegerIterator itr) {
			this.itr = itr;
		}
		
		public boolean hasNext() {
			return itr.hasNext();
		}
		
		public byte next() {
			return (byte)itr.next();
		}
		
		public void remove() {
			itr.remove();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#iterator()
	 */
	public ByteIterator byteIterator() {
		return new Itr(bits.iterator());
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#remove(byte)
	 */
	public boolean removeByte(byte v) {
		return bits.remove(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#removeAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean removeAllByte(ByteCollection a) {
		ByteSet s = a.toSet();
		
		if(s instanceof ByteBitSet) {
			return bits.removeAll(((ByteBitSet)s).bits);
		} else if(s instanceof ByteCollections.UnmodifiableSet) {
			return removeAllByte(
					((ByteCollections.UnmodifiableSet)s).wrapee);
		} else {
			return super.removeAllByte(s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#retainAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean retainAllByte(ByteCollection a) {
		ByteSet s = a.toSet();
		
		if(s instanceof ByteBitSet) {
			return bits.retainAll(((ByteBitSet)s).bits);
		} else if(s instanceof ByteCollections.UnmodifiableSet) {
			return retainAllByte(
					((ByteCollections.UnmodifiableSet)s).wrapee);
		} else {
			return super.retainAllByte(s);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#size()
	 */
	public int size() {
		return bits.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#first()
	 */
	public byte first() {
		return (byte)bits.first();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#headSet(byte)
	 */
	public ByteSortedSet headSet(byte v) {
		return collect(ByteCollections.interval(v, Byte.MAX_VALUE));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#subSet(byte, byte)
	 */
	public ByteSortedSet subSet(byte s, byte e) {
		if(s > e) {
			throw new IllegalArgumentException();
		}
		return collect(ByteCollections.interval(s, e));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#tailSet(byte)
	 */
	public ByteSortedSet tailSet(byte v) {
		return collect(ByteCollections.interval(Byte.MIN_VALUE, v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSortedSet#last()
	 */
	public byte last() {
		return (byte)bits.last();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.AbstractByteSet#collect(net.morilib.util.primitive.ByteSet)
	 */
	public ByteSortedSet collect(ByteSortedSet set) {
		ByteSortedSet r = new ByteBitSet(this);
		
		r.removeAllByte(set);
		return ByteCollections.unmodifiableSortedSet(r);
	}

}
