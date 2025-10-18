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

import java.util.Collection;
import java.util.Iterator;

import net.morilib.util.primitive.iterator.ByteIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractByteCollection
implements ByteCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean addAllByte(ByteCollection a) {
		ByteIterator i = a.byteIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addByte(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection[])
	 */
	public boolean addAllByte(ByteCollection... as) {
		boolean r = false;
		
		for(ByteCollection a : as) {
			r = addAllByte(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#addAll(java.util.Collection)
	 */
	public boolean addAllByte(Collection<? extends ByteCollection> as) {
		boolean r = false;
		
		for(ByteCollection a : as) {
			r = addAllByte(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#clear()
	 */
	public void clear() {
		ByteIterator i = byteIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#contains(byte)
	 */
	public boolean containsByte(byte v) {
		ByteIterator i = byteIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#containsAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean containsAllByte(ByteCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			ByteIterator i = a.byteIterator();
			
			while(i.hasNext()) {
				if(!containsByte(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#remove(byte)
	 */
	public boolean removeByte(byte v) {
		ByteIterator i = byteIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#removeAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean removeAllByte(ByteCollection a) {
		ByteIterator i = a.toSet().byteIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			byte b = i.next();
			
			if(containsByte(b)) {
				r = removeByte(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#retainAll(net.morilib.util.primitive.ByteCollection)
	 */
	public boolean retainAllByte(ByteCollection a) {
		ByteIterator i = byteIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			byte b = i.next();
			
			if(!a.containsByte(b)) {
				r = removeByte(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#toArray()
	 */
	public byte[] toByteArray() {
		ByteIterator i = byteIterator();
		byte[]       r = new byte[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#toArray(byte[])
	 */
	public byte[] toByteArray(byte[] a) {
		if(a.length < size()) {
			return toByteArray();
		} else {
			ByteIterator i = byteIterator();
			
			for(int j = 0; i.hasNext(); j++) {
				a[j] = i.next();
			}
			return a;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		if(o instanceof Byte) {
			return containsByte(((Byte)o).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Byte> iterator() {
		final ByteIterator i = byteIterator();
		
		return new Iterator<Byte>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Byte next() {
				return i.next();
			}

			public void remove() {
				i.remove();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#toArray()
	 */
	public Object[] toArray() {
		ByteIterator i = byteIterator();
		Byte[]      r = new Byte[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#toArray(T[])
	 */
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		Byte[] r;
		ByteIterator i;
		
		if(!(a instanceof Byte[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Byte[size()];
		} else {
			r = (Byte[])a;
		}
		
		i = byteIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Byte e) {
		return addByte(e.byteValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Byte) {
			return remove(((Byte)o).byteValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		Iterator<?> i = c.iterator();
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(!(o instanceof Byte)) {
				return false;
			} else if(!containsByte(((Byte)o).byteValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Byte> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Byte) {
					r = addByte(((Byte)o).byteValue()) | r;
				}
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(o instanceof Byte) {
				byte x = ((Byte)o).byteValue();
				if(containsByte(x)) {
					r = removeByte(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		while(i.hasNext()) {
			Object o = i.next();
			
			if(o instanceof Byte) {
				byte x = ((Byte)o).byteValue();
				if(!containsByte(x)) {
					r = removeByte(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#toSet()
	 */
	public ByteSet toSet() {
		return ByteCollections.unmodifiableSet(
				new ByteHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addByte((byte)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Byte.MIN_VALUE && v <= Byte.MAX_VALUE &&
				containsByte((byte)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Byte.MIN_VALUE || v > Byte.MAX_VALUE) {
			return false;
		}
		return removeByte((byte)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		ByteIterator i = byteIterator();
		String d = "";
		
		b.append("[");
		while(i.hasNext()) {
			b.append(d).append(i.next());
			d = ",";
		}
		b.append("]");
		return b.toString();
	}

}
