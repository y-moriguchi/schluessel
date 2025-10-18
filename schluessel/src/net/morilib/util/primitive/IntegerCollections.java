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

import net.morilib.util.primitive.iterator.IntegerIterator;
import net.morilib.util.primitive.iterator.IntegerIterators;
import net.morilib.util.primitive.iterator.IntegerVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class IntegerCollections {
	
	//
	private IntegerCollections() { }
	
	//
	private static class _Empty extends AbstractIntegerCollection {

		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllInt(IntegerCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllInt(
				Collection<? extends IntegerCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsInt(int v) {
			return false;
		}

		public boolean containsAllInt(IntegerCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeInt(int v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		public IntegerIterator intIterator() {
			return new IntegerIterator() {

				public boolean hasNext() {
					return false;
				}

				public int next() {
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
		 * @see net.morilib.util.primitive.IntegerCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final IntegerSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractIntegerCollection {
		
		//
		/*package*/ IntegerCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(IntegerCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#add(int)
		 */
		public boolean addInt(int v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection)
		 */
		public boolean addAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#addAll(net.morilib.util.primitive.IntegerCollection[])
		 */
		public boolean addAllInt(IntegerCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#addAll(java.util.Collection)
		 */
		public boolean addAllInt(
				Collection<? extends IntegerCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#contains(int)
		 */
		public boolean containsInt(int v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#containsAll(net.morilib.util.primitive.IntegerCollection)
		 */
		public boolean containsAllInt(IntegerCollection a) {
			return wrapee.containsAllInt(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements IntegerIterator {
			
			//
			private IntegerIterator itr;
			
			//
			private Itr(IntegerIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.IntegerIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.IntegerIterator#next()
			 */
			public int next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.IntegerIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#iterator()
		 */
		public IntegerIterator intIterator() {
			return new Itr(wrapee.intIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#remove(int)
		 */
		public boolean removeInt(int v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#removeAll(net.morilib.util.primitive.IntegerCollection)
		 */
		public boolean removeAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#retainAll(net.morilib.util.primitive.IntegerCollection)
		 */
		public boolean retainAllInt(IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#toArray()
		 */
		public int[] toIntArray() {
			return wrapee.toIntArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#toArray(int[])
		 */
		public int[] toIntArray(int[] a) {
			return wrapee.toIntArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerCollection#toSet()
		 */
		public IntegerSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements IntegerSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(IntegerCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSet#collect(net.morilib.util.primitive.IntegerSet)
		 */
		public IntegerSet collect(IntegerSet set) {
			return ((IntegerSet)wrapee).collect(set);
		}
		
	}
	
	public static IntegerCollection unmodifiableCollection(
			IntegerCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static IntegerSet unmodifiableSet(IntegerSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements IntegerSortedSet {

		public IntegerSet collect(IntegerSet set) {
			return this;
		}

		public IntegerSortedSet collect(IntegerSortedSet set) {
			return this;
		}

		public int first() {
			throw new NoSuchElementException();
		}

		public IntegerSortedSet headSet(int v) {
			return this;
		}

		public IntegerSortedSet subSet(int s, int e) {
			return this;
		}

		public IntegerSortedSet tailSet(int v) {
			return this;
		}

		public int last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final IntegerSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements IntegerSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(IntegerCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSet#collect(net.morilib.util.primitive.IntegerSet)
		 */
		public IntegerSortedSet collect(IntegerSortedSet set) {
			return ((IntegerSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSortedSet#first()
		 */
		public int first() {
			return ((IntegerSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSortedSet#headSet(int)
		 */
		public IntegerSortedSet headSet(int v) {
			return unmodifiableSortedSet(
					((IntegerSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSortedSet#subSet(int, int)
		 */
		public IntegerSortedSet subSet(int s, int e) {
			return unmodifiableSortedSet(
					((IntegerSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSortedSet#tailSet(int)
		 */
		public IntegerSortedSet tailSet(int v) {
			return unmodifiableSortedSet(
					((IntegerSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.IntegerSortedSet#last()
		 */
		public int last() {
			return ((IntegerSortedSet)wrapee).last();
		}
		
	}
	
	public static IntegerSortedSet unmodifiableSortedSet(
			IntegerSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractIntegerVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public int first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Integer> c) {
			throw new UnsupportedOperationException();
		}

		public Integer get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Integer set(int index, Integer element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Integer element) {
			throw new UnsupportedOperationException();
		}

		public Integer remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Integer> {

			public boolean hasNext() {
				return false;
			}

			public Integer next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Integer previous() {
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

			public void set(Integer e) {
				throw new UnsupportedOperationException();
			}

			public void add(Integer e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Integer> listIterator() {
			return ITR;
		}

		public ListIterator<Integer> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Integer> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addInt(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllInt(int index, IntegerCollection a) {
			throw new UnsupportedOperationException();
		}

		public int getInt(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfInt(int v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public int removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public IntegerList rest() {
			throw new IndexOutOfBoundsException();
		}

		public IntegerList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public int setInt(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfInt(int v) {
			return -1;
		}

		public IntegerVectorIterator intVectorIterator() {
			return IntegerIterators.NULL_ITERATOR;
		}

		public IntegerVectorIterator intVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return intVectorIterator();
		}

		public IntegerVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final IntegerVector EMPTY_VECTOR = new _EmptyV();
	
}
