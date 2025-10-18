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

import net.morilib.util.primitive.iterator.LongIterator;
import net.morilib.util.primitive.iterator.LongIterators;
import net.morilib.util.primitive.iterator.LongVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class LongCollections {
	
	//
	private LongCollections() { }
	
	//
	private static class _Empty extends AbstractLongCollection {

		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllLong(LongCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllLong(
				Collection<? extends LongCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsLong(long v) {
			return false;
		}

		public boolean containsAllLong(LongCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeLong(long v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		public LongIterator longIterator() {
			return new LongIterator() {

				public boolean hasNext() {
					return false;
				}

				public long next() {
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
		 * @see net.morilib.util.primitive.LongCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final LongSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractLongCollection {
		
		//
		/*package*/ LongCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(LongCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#add(long)
		 */
		public boolean addLong(long v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection)
		 */
		public boolean addAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#addAll(net.morilib.util.primitive.LongCollection[])
		 */
		public boolean addAllLong(LongCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#addAll(java.util.Collection)
		 */
		public boolean addAllLong(
				Collection<? extends LongCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#contains(long)
		 */
		public boolean containsLong(long v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#containsAll(net.morilib.util.primitive.LongCollection)
		 */
		public boolean containsAllLong(LongCollection a) {
			return wrapee.containsAllLong(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements LongIterator {
			
			//
			private LongIterator itr;
			
			//
			private Itr(LongIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.LongIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.LongIterator#next()
			 */
			public long next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.LongIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#iterator()
		 */
		public LongIterator longIterator() {
			return new Itr(wrapee.longIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#remove(long)
		 */
		public boolean removeLong(long v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#removeAll(net.morilib.util.primitive.LongCollection)
		 */
		public boolean removeAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#retainAll(net.morilib.util.primitive.LongCollection)
		 */
		public boolean retainAllLong(LongCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#toArray()
		 */
		public long[] toLongArray() {
			return wrapee.toLongArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#toArray(long[])
		 */
		public long[] toLongArray(long[] a) {
			return wrapee.toLongArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongCollection#toSet()
		 */
		public LongSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements LongSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(LongCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSet#collect(net.morilib.util.primitive.LongSet)
		 */
		public LongSet collect(LongSet set) {
			return ((LongSet)wrapee).collect(set);
		}
		
	}
	
	public static LongCollection unmodifiableCollection(
			LongCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static LongSet unmodifiableSet(LongSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements LongSortedSet {

		public LongSet collect(LongSet set) {
			return this;
		}

		public LongSortedSet collect(LongSortedSet set) {
			return this;
		}

		public long first() {
			throw new NoSuchElementException();
		}

		public LongSortedSet headSet(long v) {
			return this;
		}

		public LongSortedSet subSet(long s, long e) {
			return this;
		}

		public LongSortedSet tailSet(long v) {
			return this;
		}

		public long last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final LongSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements LongSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(LongCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSet#collect(net.morilib.util.primitive.LongSet)
		 */
		public LongSortedSet collect(LongSortedSet set) {
			return ((LongSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSortedSet#first()
		 */
		public long first() {
			return ((LongSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSortedSet#headSet(long)
		 */
		public LongSortedSet headSet(long v) {
			return unmodifiableSortedSet(
					((LongSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSortedSet#subSet(long, long)
		 */
		public LongSortedSet subSet(long s, long e) {
			return unmodifiableSortedSet(
					((LongSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSortedSet#tailSet(long)
		 */
		public LongSortedSet tailSet(long v) {
			return unmodifiableSortedSet(
					((LongSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.LongSortedSet#last()
		 */
		public long last() {
			return ((LongSortedSet)wrapee).last();
		}
		
	}
	
	public static LongSortedSet unmodifiableSortedSet(
			LongSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractLongVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public long first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Long> c) {
			throw new UnsupportedOperationException();
		}

		public Long get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Long set(int index, Long element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Long element) {
			throw new UnsupportedOperationException();
		}

		public Long remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Long> {

			public boolean hasNext() {
				return false;
			}

			public Long next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Long previous() {
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

			public void set(Long e) {
				throw new UnsupportedOperationException();
			}

			public void add(Long e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Long> listIterator() {
			return ITR;
		}

		public ListIterator<Long> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Long> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addLong(int index, long v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllLong(int index, LongCollection a) {
			throw new UnsupportedOperationException();
		}

		public long getLong(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfLong(long v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public long removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public LongList rest() {
			throw new IndexOutOfBoundsException();
		}

		public LongList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public long setLong(int index, long v) {
			throw new UnsupportedOperationException();
		}

		public long set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfLong(long v) {
			return -1;
		}

		public LongVectorIterator longVectorIterator() {
			return LongIterators.NULL_ITERATOR;
		}

		public LongVectorIterator longVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return longVectorIterator();
		}

		public LongVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final LongVector EMPTY_VECTOR = new _EmptyV();
	
}
