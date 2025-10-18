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

import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractFloatCollection
implements FloatCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection)
	 */
	public boolean addAllFloat(FloatCollection a) {
		FloatIterator i = a.floatIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addFloat(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection[])
	 */
	public boolean addAllFloat(FloatCollection... as) {
		boolean r = false;
		
		for(FloatCollection a : as) {
			r = addAllFloat(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#addAll(java.util.Collection)
	 */
	public boolean addAllFloat(Collection<? extends FloatCollection> as) {
		boolean r = false;
		
		for(FloatCollection a : as) {
			r = addAllFloat(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#clear()
	 */
	public void clear() {
		FloatIterator i = floatIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#contains(float)
	 */
	public boolean containsFloat(float v) {
		FloatIterator i = floatIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#containsAll(net.morilib.util.primitive.FloatCollection)
	 */
	public boolean containsAllFloat(FloatCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			FloatIterator i = a.floatIterator();
			
			while(i.hasNext()) {
				if(!containsFloat(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#remove(float)
	 */
	public boolean removeFloat(float v) {
		FloatIterator i = floatIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#removeAll(net.morilib.util.primitive.FloatCollection)
	 */
	public boolean removeAllFloat(FloatCollection a) {
		FloatIterator i = a.toSet().floatIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			float b = i.next();
			
			if(containsFloat(b)) {
				r = removeFloat(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#retainAll(net.morilib.util.primitive.FloatCollection)
	 */
	public boolean retainAllFloat(FloatCollection a) {
		FloatIterator i = floatIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			float b = i.next();
			
			if(!a.containsFloat(b)) {
				r = removeFloat(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#toArray()
	 */
	public float[] toFloatArray() {
		FloatIterator i = floatIterator();
		float[]       r = new float[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#toArray(float[])
	 */
	public float[] toFloatArray(float[] a) {
		if(a.length < size()) {
			return toFloatArray();
		} else {
			FloatIterator i = floatIterator();
			
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
		if(o instanceof Float) {
			return containsFloat(((Float)o).floatValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Float> iterator() {
		final FloatIterator i = floatIterator();
		
		return new Iterator<Float>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Float next() {
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
		FloatIterator i = floatIterator();
		Float[]      r = new Float[size()];
		
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
		Float[] r;
		FloatIterator i;
		
		if(!(a instanceof Float[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Float[size()];
		} else {
			r = (Float[])a;
		}
		
		i = floatIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Float e) {
		return addFloat(e.floatValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Float) {
			return remove(((Float)o).floatValue());
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
			
			if(!(o instanceof Float)) {
				return false;
			} else if(!containsFloat(((Float)o).floatValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Float> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Float) {
					r = addFloat(((Float)o).floatValue()) | r;
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
			
			if(o instanceof Float) {
				float x = ((Float)o).floatValue();
				if(containsFloat(x)) {
					r = removeFloat(x) | r;
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
			
			if(o instanceof Float) {
				float x = ((Float)o).floatValue();
				if(!containsFloat(x)) {
					r = removeFloat(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#toSet()
	 */
	public FloatSet toSet() {
		return FloatCollections.unmodifiableSet(
				new FloatHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addFloat((float)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Float.MIN_VALUE && v <= Float.MAX_VALUE &&
				containsFloat((float)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Float.MIN_VALUE || v > Float.MAX_VALUE) {
			return false;
		}
		return removeFloat((float)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		FloatIterator i = floatIterator();
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
