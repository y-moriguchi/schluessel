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

import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractIntegerCollection
implements IntegerCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean addAllInt(IntegerCollection a) {
		IntegerIterator i = a.intIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addInt(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection[])
	 */
	public boolean addAllInt(IntegerCollection... as) {
		boolean r = false;
		
		for(IntegerCollection a : as) {
			r = addAllInt(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#addAll(java.util.Collection)
	 */
	public boolean addAllInt(Collection<? extends IntegerCollection> as) {
		boolean r = false;
		
		for(IntegerCollection a : as) {
			r = addAllInt(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#clear()
	 */
	public void clear() {
		IntegerIterator i = intIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#contains(int)
	 */
	public boolean containsInt(int v) {
		IntegerIterator i = intIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#containsAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean containsAllInt(IntegerCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			IntegerIterator i = a.intIterator();
			
			while(i.hasNext()) {
				if(!containsInt(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#remove(int)
	 */
	public boolean removeInt(int v) {
		IntegerIterator i = intIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#removeAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean removeAllInt(IntegerCollection a) {
		IntegerIterator i = a.toSet().intIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			int b = i.next();
			
			if(containsInt(b)) {
				r = removeInt(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#retainAll(net.morilib.util.primitive.IntegerCollection)
	 */
	public boolean retainAllInt(IntegerCollection a) {
		IntegerIterator i = intIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			int b = i.next();
			
			if(!a.containsInt(b)) {
				r = removeInt(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toArray()
	 */
	public int[] toIntArray() {
		IntegerIterator i = intIterator();
		int[]       r = new int[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toArray(int[])
	 */
	public int[] toIntArray(int[] a) {
		if(a.length < size()) {
			return toIntArray();
		} else {
			IntegerIterator i = intIterator();
			
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
		if(o instanceof Integer) {
			return containsInt(((Integer)o).intValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Integer> iterator() {
		final IntegerIterator i = intIterator();
		
		return new Iterator<Integer>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Integer next() {
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
		IntegerIterator i = intIterator();
		Integer[]      r = new Integer[size()];
		
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
		Integer[] r;
		IntegerIterator i;
		
		if(!(a instanceof Integer[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Integer[size()];
		} else {
			r = (Integer[])a;
		}
		
		i = intIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Integer e) {
		return addInt(e.intValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Integer) {
			return remove(((Integer)o).intValue());
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
			
			if(!(o instanceof Integer)) {
				return false;
			} else if(!containsInt(((Integer)o).intValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Integer> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Integer) {
					r = addInt(((Integer)o).intValue()) | r;
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
			
			if(o instanceof Integer) {
				int x = ((Integer)o).intValue();
				if(containsInt(x)) {
					r = removeInt(x) | r;
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
			
			if(o instanceof Integer) {
				int x = ((Integer)o).intValue();
				if(!containsInt(x)) {
					r = removeInt(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toSet()
	 */
	public IntegerSet toSet() {
		return IntegerCollections.unmodifiableSet(
				new IntegerHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addInt((int)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Integer.MIN_VALUE && v <= Integer.MAX_VALUE &&
				containsInt((int)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Integer.MIN_VALUE || v > Integer.MAX_VALUE) {
			return false;
		}
		return removeInt((int)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		IntegerIterator i = intIterator();
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
