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

import net.morilib.util.primitive.iterator.FloatIterator;
import net.morilib.util.primitive.iterator.FloatIterators;
import net.morilib.util.primitive.iterator.FloatVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class FloatCollections {
	
	//
	private FloatCollections() { }
	
	//
	private static class _Empty extends AbstractFloatCollection {

		public boolean addFloat(float v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllFloat(FloatCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllFloat(
				Collection<? extends FloatCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsFloat(float v) {
			return false;
		}

		public boolean containsAllFloat(FloatCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeFloat(float v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		public FloatIterator floatIterator() {
			return new FloatIterator() {

				public boolean hasNext() {
					return false;
				}

				public float next() {
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
		 * @see net.morilib.util.primitive.FloatCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final FloatSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractFloatCollection {
		
		//
		/*package*/ FloatCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(FloatCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#add(float)
		 */
		public boolean addFloat(float v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection)
		 */
		public boolean addAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#addAll(net.morilib.util.primitive.FloatCollection[])
		 */
		public boolean addAllFloat(FloatCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#addAll(java.util.Collection)
		 */
		public boolean addAllFloat(
				Collection<? extends FloatCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#contains(float)
		 */
		public boolean containsFloat(float v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#containsAll(net.morilib.util.primitive.FloatCollection)
		 */
		public boolean containsAllFloat(FloatCollection a) {
			return wrapee.containsAllFloat(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements FloatIterator {
			
			//
			private FloatIterator itr;
			
			//
			private Itr(FloatIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.FloatIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.FloatIterator#next()
			 */
			public float next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.FloatIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#iterator()
		 */
		public FloatIterator floatIterator() {
			return new Itr(wrapee.floatIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#remove(float)
		 */
		public boolean removeFloat(float v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#removeAll(net.morilib.util.primitive.FloatCollection)
		 */
		public boolean removeAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#retainAll(net.morilib.util.primitive.FloatCollection)
		 */
		public boolean retainAllFloat(FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#toArray()
		 */
		public float[] toFloatArray() {
			return wrapee.toFloatArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#toArray(float[])
		 */
		public float[] toFloatArray(float[] a) {
			return wrapee.toFloatArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatCollection#toSet()
		 */
		public FloatSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements FloatSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(FloatCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSet#collect(net.morilib.util.primitive.FloatSet)
		 */
		public FloatSet collect(FloatSet set) {
			return ((FloatSet)wrapee).collect(set);
		}
		
	}
	
	public static FloatCollection unmodifiableCollection(
			FloatCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static FloatSet unmodifiableSet(FloatSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements FloatSortedSet {

		public FloatSet collect(FloatSet set) {
			return this;
		}

		public FloatSortedSet collect(FloatSortedSet set) {
			return this;
		}

		public float first() {
			throw new NoSuchElementException();
		}

		public FloatSortedSet headSet(float v) {
			return this;
		}

		public FloatSortedSet subSet(float s, float e) {
			return this;
		}

		public FloatSortedSet tailSet(float v) {
			return this;
		}

		public float last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final FloatSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements FloatSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(FloatCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSet#collect(net.morilib.util.primitive.FloatSet)
		 */
		public FloatSortedSet collect(FloatSortedSet set) {
			return ((FloatSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSortedSet#first()
		 */
		public float first() {
			return ((FloatSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSortedSet#headSet(float)
		 */
		public FloatSortedSet headSet(float v) {
			return unmodifiableSortedSet(
					((FloatSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSortedSet#subSet(float, float)
		 */
		public FloatSortedSet subSet(float s, float e) {
			return unmodifiableSortedSet(
					((FloatSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSortedSet#tailSet(float)
		 */
		public FloatSortedSet tailSet(float v) {
			return unmodifiableSortedSet(
					((FloatSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.FloatSortedSet#last()
		 */
		public float last() {
			return ((FloatSortedSet)wrapee).last();
		}
		
	}
	
	public static FloatSortedSet unmodifiableSortedSet(
			FloatSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractFloatVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public float first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Float> c) {
			throw new UnsupportedOperationException();
		}

		public Float get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Float set(int index, Float element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Float element) {
			throw new UnsupportedOperationException();
		}

		public Float remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Float> {

			public boolean hasNext() {
				return false;
			}

			public Float next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Float previous() {
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

			public void set(Float e) {
				throw new UnsupportedOperationException();
			}

			public void add(Float e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Float> listIterator() {
			return ITR;
		}

		public ListIterator<Float> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Float> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addFloat(int index, float v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllFloat(int index, FloatCollection a) {
			throw new UnsupportedOperationException();
		}

		public float getFloat(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfFloat(float v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public float removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public FloatList rest() {
			throw new IndexOutOfBoundsException();
		}

		public FloatList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public float setFloat(int index, float v) {
			throw new UnsupportedOperationException();
		}

		public float set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfFloat(float v) {
			return -1;
		}

		public FloatVectorIterator floatVectorIterator() {
			return FloatIterators.NULL_ITERATOR;
		}

		public FloatVectorIterator floatVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return floatVectorIterator();
		}

		public FloatVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final FloatVector EMPTY_VECTOR = new _EmptyV();
	
}
