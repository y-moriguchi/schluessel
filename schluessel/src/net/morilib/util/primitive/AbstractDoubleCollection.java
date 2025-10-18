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

import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractDoubleCollection
implements DoubleCollection {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean addAllDouble(DoubleCollection a) {
		DoubleIterator i = a.doubleIterator();
		boolean r = false;
		
		if(a.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				r = addDouble(i.next()) | r;
			}
			return r;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection[])
	 */
	public boolean addAllDouble(DoubleCollection... as) {
		boolean r = false;
		
		for(DoubleCollection a : as) {
			r = addAllDouble(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#addAll(java.util.Collection)
	 */
	public boolean addAllDouble(Collection<? extends DoubleCollection> as) {
		boolean r = false;
		
		for(DoubleCollection a : as) {
			r = addAllDouble(a) | r;
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#clear()
	 */
	public void clear() {
		DoubleIterator i = doubleIterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#contains(double)
	 */
	public boolean containsDouble(double v) {
		DoubleIterator i = doubleIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#containsAll(net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean containsAllDouble(DoubleCollection a) {
		if(!isInfinite() && a.isInfinite()) {
			return false;
		} else {
			DoubleIterator i = a.doubleIterator();
			
			while(i.hasNext()) {
				if(!containsDouble(i.next())) {
					return false;
				}
			}
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#remove(double)
	 */
	public boolean removeDouble(double v) {
		DoubleIterator i = doubleIterator();
		
		while(i.hasNext()) {
			if(i.next() == v) {
				i.remove();
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#removeAll(net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean removeAllDouble(DoubleCollection a) {
		DoubleIterator i = a.toSet().doubleIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			double b = i.next();
			
			if(containsDouble(b)) {
				r = removeDouble(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#retainAll(net.morilib.util.primitive.DoubleCollection)
	 */
	public boolean retainAllDouble(DoubleCollection a) {
		DoubleIterator i = doubleIterator();
		boolean r = false;
		
		while(i.hasNext()) {
			double b = i.next();
			
			if(!a.containsDouble(b)) {
				r = removeDouble(b) | r;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#toArray()
	 */
	public double[] toDoubleArray() {
		DoubleIterator i = doubleIterator();
		double[]       r = new double[size()];
		
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#toArray(double[])
	 */
	public double[] toDoubleArray(double[] a) {
		if(a.length < size()) {
			return toDoubleArray();
		} else {
			DoubleIterator i = doubleIterator();
			
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
		if(o instanceof Double) {
			return containsDouble(((Double)o).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#iterator()
	 */
	public Iterator<Double> iterator() {
		final DoubleIterator i = doubleIterator();
		
		return new Iterator<Double>() {

			public boolean hasNext() {
				return i.hasNext();
			}

			public Double next() {
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
		DoubleIterator i = doubleIterator();
		Double[]      r = new Double[size()];
		
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
		Double[] r;
		DoubleIterator i;
		
		if(!(a instanceof Double[])) {
			throw new ClassCastException();
		} else if(a.length < size()) {
			r = new Double[size()];
		} else {
			r = (Double[])a;
		}
		
		i = doubleIterator();
		for(int j = 0; i.hasNext(); j++) {
			r[j] = i.next();
		}
		return (T[])r;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#add(java.lang.Object)
	 */
	public boolean add(Double e) {
		return addDouble(e.doubleValue());
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Double) {
			return remove(((Double)o).doubleValue());
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
			
			if(!(o instanceof Double)) {
				return false;
			} else if(!containsDouble(((Double)o).doubleValue())) {
				return false;
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Collection#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Double> c) {
		Iterator<?> i = iterator();
		boolean r = false;
		
		if(c.isEmpty()) {
			return false;
		} else {
			while(i.hasNext()) {
				Object o = i.next();
				
				if(o instanceof Double) {
					r = addDouble(((Double)o).doubleValue()) | r;
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
			
			if(o instanceof Double) {
				double x = ((Double)o).doubleValue();
				if(containsDouble(x)) {
					r = removeDouble(x) | r;
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
			
			if(o instanceof Double) {
				double x = ((Double)o).doubleValue();
				if(!containsDouble(x)) {
					r = removeDouble(x) | r;
				}
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#toSet()
	 */
	public DoubleSet toSet() {
		return DoubleCollections.unmodifiableSet(
				new DoubleHashSet(this));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#add(int)
	 */
	public boolean add(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return addDouble((double)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#contains(int)
	 */
	public boolean contains(int v) {
		return (v >= Double.MIN_VALUE && v <= Double.MAX_VALUE &&
				containsDouble((double)v));
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#removeElement(int)
	 */
	public boolean removeElement(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			return false;
		}
		return removeDouble((double)v);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuffer b = new StringBuffer();
		DoubleIterator i = doubleIterator();
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
