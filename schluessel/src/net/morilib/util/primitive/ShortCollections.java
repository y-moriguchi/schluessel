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

import net.morilib.util.primitive.iterator.ShortIterator;
import net.morilib.util.primitive.iterator.ShortIterators;
import net.morilib.util.primitive.iterator.ShortVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class ShortCollections {
	
	//
	private ShortCollections() { }
	
	//
	private static class _Empty extends AbstractShortCollection {

		public boolean addShort(short v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllShort(ShortCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllShort(
				Collection<? extends ShortCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsShort(short v) {
			return false;
		}

		public boolean containsAllShort(ShortCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeShort(short v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		public ShortIterator shortIterator() {
			return new ShortIterator() {

				public boolean hasNext() {
					return false;
				}

				public short next() {
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
		 * @see net.morilib.util.primitive.ShortCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final ShortSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractShortCollection {
		
		//
		/*package*/ ShortCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(ShortCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#add(short)
		 */
		public boolean addShort(short v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection)
		 */
		public boolean addAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#addAll(net.morilib.util.primitive.ShortCollection[])
		 */
		public boolean addAllShort(ShortCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#addAll(java.util.Collection)
		 */
		public boolean addAllShort(
				Collection<? extends ShortCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#contains(short)
		 */
		public boolean containsShort(short v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#containsAll(net.morilib.util.primitive.ShortCollection)
		 */
		public boolean containsAllShort(ShortCollection a) {
			return wrapee.containsAllShort(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements ShortIterator {
			
			//
			private ShortIterator itr;
			
			//
			private Itr(ShortIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ShortIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ShortIterator#next()
			 */
			public short next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ShortIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#iterator()
		 */
		public ShortIterator shortIterator() {
			return new Itr(wrapee.shortIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#remove(short)
		 */
		public boolean removeShort(short v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#removeAll(net.morilib.util.primitive.ShortCollection)
		 */
		public boolean removeAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#retainAll(net.morilib.util.primitive.ShortCollection)
		 */
		public boolean retainAllShort(ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#toArray()
		 */
		public short[] toShortArray() {
			return wrapee.toShortArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#toArray(short[])
		 */
		public short[] toShortArray(short[] a) {
			return wrapee.toShortArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortCollection#toSet()
		 */
		public ShortSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements ShortSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(ShortCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSet#collect(net.morilib.util.primitive.ShortSet)
		 */
		public ShortSet collect(ShortSet set) {
			return ((ShortSet)wrapee).collect(set);
		}
		
	}
	
	public static ShortCollection unmodifiableCollection(
			ShortCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static ShortSet unmodifiableSet(ShortSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements ShortSortedSet {

		public ShortSet collect(ShortSet set) {
			return this;
		}

		public ShortSortedSet collect(ShortSortedSet set) {
			return this;
		}

		public short first() {
			throw new NoSuchElementException();
		}

		public ShortSortedSet headSet(short v) {
			return this;
		}

		public ShortSortedSet subSet(short s, short e) {
			return this;
		}

		public ShortSortedSet tailSet(short v) {
			return this;
		}

		public short last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final ShortSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements ShortSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(ShortCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSet#collect(net.morilib.util.primitive.ShortSet)
		 */
		public ShortSortedSet collect(ShortSortedSet set) {
			return ((ShortSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSortedSet#first()
		 */
		public short first() {
			return ((ShortSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSortedSet#headSet(short)
		 */
		public ShortSortedSet headSet(short v) {
			return unmodifiableSortedSet(
					((ShortSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSortedSet#subSet(short, short)
		 */
		public ShortSortedSet subSet(short s, short e) {
			return unmodifiableSortedSet(
					((ShortSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSortedSet#tailSet(short)
		 */
		public ShortSortedSet tailSet(short v) {
			return unmodifiableSortedSet(
					((ShortSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ShortSortedSet#last()
		 */
		public short last() {
			return ((ShortSortedSet)wrapee).last();
		}
		
	}
	
	public static ShortSortedSet unmodifiableSortedSet(
			ShortSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractShortVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public short first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Short> c) {
			throw new UnsupportedOperationException();
		}

		public Short get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Short set(int index, Short element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Short element) {
			throw new UnsupportedOperationException();
		}

		public Short remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Short> {

			public boolean hasNext() {
				return false;
			}

			public Short next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Short previous() {
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

			public void set(Short e) {
				throw new UnsupportedOperationException();
			}

			public void add(Short e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Short> listIterator() {
			return ITR;
		}

		public ListIterator<Short> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Short> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addShort(int index, short v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllShort(int index, ShortCollection a) {
			throw new UnsupportedOperationException();
		}

		public short getShort(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfShort(short v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public short removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public ShortList rest() {
			throw new IndexOutOfBoundsException();
		}

		public ShortList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public short setShort(int index, short v) {
			throw new UnsupportedOperationException();
		}

		public short set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfShort(short v) {
			return -1;
		}

		public ShortVectorIterator shortVectorIterator() {
			return ShortIterators.NULL_ITERATOR;
		}

		public ShortVectorIterator shortVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return shortVectorIterator();
		}

		public ShortVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final ShortVector EMPTY_VECTOR = new _EmptyV();
	
}
