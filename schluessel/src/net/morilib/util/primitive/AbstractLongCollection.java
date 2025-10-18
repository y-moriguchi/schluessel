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

import net.morilib.util.primitive.iterator.LongIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractLongCollection
implements LongCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean addAllLong(LongCollection a) {
		LongIterator i = a.longIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addLong(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection[])
	 */
	public boolean addAllLong(LongCollection... as) {
		boolean r = false;
		
		for(LongCollection a : as) {
			r = addAllLong(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#addAll(java.util.Collection)
	 */
	public boolean addAllLong(Collection<? extends LongCollection> as) {
		boolean r = false;
		
		for(LongCollection a : as) {
			r = addAllLong(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#clear()
	 */
	public void clear() {
		LongIterator i = longIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#contains(long)
	 */
	public boolean containsLong(long v) {
		LongIterator i = longIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#containsAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean containsAllLong(LongCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			LongIterator i = a.longIterator();
			
			while(i.hasNext()) {
				if(!containsLong(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#remove(long)
	 */
	public boolean removeLong(long v) {
		LongIterator i = longIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#removeAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean removeAllLong(LongCollection a) {
		LongIterator i = a.toSet().longIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			long b = i.next();
			
			if(containsLong(b)) {
				r = removeLong(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#retainAll(net.morilib.util.primitive.LongCollection)
	 */
	public boolean retainAllLong(LongCollection a) {
		LongIterator i = longIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			long b = i.next();
			
			if(!a.containsLong(b)) {
				r = removeLong(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toArray()
	 */
	public long[] toLongArray() {
		LongIterator i = longIterator();
		long[]       r = new long[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toArray(long[])
	 */
	public long[] toLongArray(long[] a) {
		if(a.length < size()) {
			return toLongArray();
		} else {
			LongIterator i = longIterator();
			
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
		if(o instanceof Long) {
			return containsLong(((Long)o).longValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Long> iterator() {
		final LongIterator i = longIterator();
		
		return new Iterator<Long>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Long next() {
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
		LongIterator i = longIterator();
		Long[]      r = new Long[size()];
		
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
		Long[] r;
		LongIterator i;
		
		if(!(a instanceof Long[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Long[size()];
		} else {
			r = (Long[])a;
		}
		
		i = longIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Long e) {
		return addLong(e.longValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Long) {
			return remove(((Long)o).longValue());
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
			
			if(!(o instanceof Long)) {
				return false;
			} else if(!containsLong(((Long)o).longValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Long> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Long) {
					r = addLong(((Long)o).longValue()) | r;
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
			
			if(o instanceof Long) {
				long x = ((Long)o).longValue();
				if(containsLong(x)) {
					r = removeLong(x) | r;
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
			
			if(o instanceof Long) {
				long x = ((Long)o).longValue();
				if(!containsLong(x)) {
					r = removeLong(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toSet()
	 */
	public LongSet toSet() {
		return LongCollections.unmodifiableSet(
				new LongHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addLong((long)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Long.MIN_VALUE && v <= Long.MAX_VALUE &&
				containsLong((long)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Long.MIN_VALUE || v > Long.MAX_VALUE) {
			return false;
		}
		return removeLong((long)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		LongIterator i = longIterator();
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
