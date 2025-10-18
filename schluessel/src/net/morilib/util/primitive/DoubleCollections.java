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
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

import net.morilib.util.primitive.iterator.DoubleIterator;
import net.morilib.util.primitive.iterator.DoubleIterators;
import net.morilib.util.primitive.iterator.DoubleVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class DoubleCollections {
	
	//
	private DoubleCollections() { }
	
	//
	private static class _Empty extends AbstractDoubleCollection {

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllDouble(DoubleCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllDouble(
				Collection<? extends DoubleCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsDouble(double v) {
			return false;
		}

		public boolean containsAllDouble(DoubleCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeDouble(double v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		public DoubleIterator doubleIterator() {
			return new DoubleIterator() {

				public boolean hasNext() {
					return false;
				}

				public double next() {
					throw new NoSuchElementException();
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
				
			};
		}

		public int size() {
			return 0;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final DoubleSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractDoubleCollection {
		
		//
		/*package*/ DoubleCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(DoubleCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#add(double)
		 */
		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection)
		 */
		public boolean addAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#addAll(net.morilib.util.primitive.DoubleCollection[])
		 */
		public boolean addAllDouble(DoubleCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#addAll(java.util.Collection)
		 */
		public boolean addAllDouble(
				Collection<? extends DoubleCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#contains(double)
		 */
		public boolean containsDouble(double v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#containsAll(net.morilib.util.primitive.DoubleCollection)
		 */
		public boolean containsAllDouble(DoubleCollection a) {
			return wrapee.containsAllDouble(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements DoubleIterator {
			
			//
			private DoubleIterator itr;
			
			//
			private Itr(DoubleIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.DoubleIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.DoubleIterator#next()
			 */
			public double next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.DoubleIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#iterator()
		 */
		public DoubleIterator doubleIterator() {
			return new Itr(wrapee.doubleIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#remove(double)
		 */
		public boolean removeDouble(double v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#removeAll(net.morilib.util.primitive.DoubleCollection)
		 */
		public boolean removeAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#retainAll(net.morilib.util.primitive.DoubleCollection)
		 */
		public boolean retainAllDouble(DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#toArray()
		 */
		public double[] toDoubleArray() {
			return wrapee.toDoubleArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#toArray(double[])
		 */
		public double[] toDoubleArray(double[] a) {
			return wrapee.toDoubleArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleCollection#toSet()
		 */
		public DoubleSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements DoubleSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(DoubleCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSet#collect(net.morilib.util.primitive.DoubleSet)
		 */
		public DoubleSet collect(DoubleSet set) {
			return ((DoubleSet)wrapee).collect(set);
		}
		
	}
	
	public static DoubleCollection unmodifiableCollection(
			DoubleCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static DoubleSet unmodifiableSet(DoubleSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements DoubleSortedSet {

		public DoubleSet collect(DoubleSet set) {
			return this;
		}

		public DoubleSortedSet collect(DoubleSortedSet set) {
			return this;
		}

		public double first() {
			throw new NoSuchElementException();
		}

		public DoubleSortedSet headSet(double v) {
			return this;
		}

		public DoubleSortedSet subSet(double s, double e) {
			return this;
		}

		public DoubleSortedSet tailSet(double v) {
			return this;
		}

		public double last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final DoubleSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements DoubleSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(DoubleCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSet#collect(net.morilib.util.primitive.DoubleSet)
		 */
		public DoubleSortedSet collect(DoubleSortedSet set) {
			return ((DoubleSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSortedSet#first()
		 */
		public double first() {
			return ((DoubleSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSortedSet#headSet(double)
		 */
		public DoubleSortedSet headSet(double v) {
			return unmodifiableSortedSet(
					((DoubleSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSortedSet#subSet(double, double)
		 */
		public DoubleSortedSet subSet(double s, double e) {
			return unmodifiableSortedSet(
					((DoubleSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSortedSet#tailSet(double)
		 */
		public DoubleSortedSet tailSet(double v) {
			return unmodifiableSortedSet(
					((DoubleSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.DoubleSortedSet#last()
		 */
		public double last() {
			return ((DoubleSortedSet)wrapee).last();
		}
		
	}
	
	public static DoubleSortedSet unmodifiableSortedSet(
			DoubleSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractDoubleVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public double first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Double> c) {
			throw new UnsupportedOperationException();
		}

		public Double get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Double set(int index, Double element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Double element) {
			throw new UnsupportedOperationException();
		}

		public Double remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Double> {

			public boolean hasNext() {
				return false;
			}

			public Double next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Double previous() {
				throw new NoSuchElementException();
			}

			public int nextIndex() {
				return 0;
			}

			public int previousIndex() {
				return -1;
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

			public void set(Double e) {
				throw new UnsupportedOperationException();
			}

			public void add(Double e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Double> listIterator() {
			return ITR;
		}

		public ListIterator<Double> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Double> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addDouble(int index, double v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllDouble(int index, DoubleCollection a) {
			throw new UnsupportedOperationException();
		}

		public double getDouble(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfDouble(double v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public double removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public DoubleList rest() {
			throw new IndexOutOfBoundsException();
		}

		public DoubleList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public double setDouble(int index, double v) {
			throw new UnsupportedOperationException();
		}

		public double set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfDouble(double v) {
			return -1;
		}

		public DoubleVectorIterator doubleVectorIterator() {
			return DoubleIterators.NULL_ITERATOR;
		}

		public DoubleVectorIterator doubleVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return doubleVectorIterator();
		}

		public DoubleVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final DoubleVector EMPTY_VECTOR = new _EmptyV();
	
}
