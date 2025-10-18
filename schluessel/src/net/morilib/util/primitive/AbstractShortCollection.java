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

import net.morilib.util.primitive.iterator.ShortIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractShortCollection
implements ShortCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean addAllShort(ShortCollection a) {
		ShortIterator i = a.shortIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addShort(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection[])
	 */
	public boolean addAllShort(ShortCollection... as) {
		boolean r = false;
		
		for(ShortCollection a : as) {
			r = addAllShort(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#addAll(java.util.Collection)
	 */
	public boolean addAllShort(Collection<? extends ShortCollection> as) {
		boolean r = false;
		
		for(ShortCollection a : as) {
			r = addAllShort(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#clear()
	 */
	public void clear() {
		ShortIterator i = shortIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#contains(short)
	 */
	public boolean containsShort(short v) {
		ShortIterator i = shortIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#containsAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean containsAllShort(ShortCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			ShortIterator i = a.shortIterator();
			
			while(i.hasNext()) {
				if(!containsShort(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#remove(short)
	 */
	public boolean removeShort(short v) {
		ShortIterator i = shortIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#removeAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean removeAllShort(ShortCollection a) {
		ShortIterator i = a.toSet().shortIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			short b = i.next();
			
			if(containsShort(b)) {
				r = removeShort(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#retainAll(net.morilib.util.primitive.ShortCollection)
	 */
	public boolean retainAllShort(ShortCollection a) {
		ShortIterator i = shortIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			short b = i.next();
			
			if(!a.containsShort(b)) {
				r = removeShort(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toArray()
	 */
	public short[] toShortArray() {
		ShortIterator i = shortIterator();
		short[]       r = new short[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toArray(short[])
	 */
	public short[] toShortArray(short[] a) {
		if(a.length < size()) {
			return toShortArray();
		} else {
			ShortIterator i = shortIterator();
			
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
		if(o instanceof Short) {
			return containsShort(((Short)o).shortValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Short> iterator() {
		final ShortIterator i = shortIterator();
		
		return new Iterator<Short>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Short next() {
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
		ShortIterator i = shortIterator();
		Short[]      r = new Short[size()];
		
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
		Short[] r;
		ShortIterator i;
		
		if(!(a instanceof Short[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Short[size()];
		} else {
			r = (Short[])a;
		}
		
		i = shortIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Short e) {
		return addShort(e.shortValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Short) {
			return remove(((Short)o).shortValue());
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
			
			if(!(o instanceof Short)) {
				return false;
			} else if(!containsShort(((Short)o).shortValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Short> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Short) {
					r = addShort(((Short)o).shortValue()) | r;
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
			
			if(o instanceof Short) {
				short x = ((Short)o).shortValue();
				if(containsShort(x)) {
					r = removeShort(x) | r;
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
			
			if(o instanceof Short) {
				short x = ((Short)o).shortValue();
				if(!containsShort(x)) {
					r = removeShort(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toSet()
	 */
	public ShortSet toSet() {
		return ShortCollections.unmodifiableSet(
				new ShortHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addShort((short)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Short.MIN_VALUE && v <= Short.MAX_VALUE &&
				containsShort((short)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Short.MIN_VALUE || v > Short.MAX_VALUE) {
			return false;
		}
		return removeShort((short)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		ShortIterator i = shortIterator();
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
