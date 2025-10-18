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

import net.morilib.util.primitive.iterator.ByteIterator;
import net.morilib.util.primitive.iterator.ByteIterators;
import net.morilib.util.primitive.iterator.ByteVectorIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class ByteCollections {
	
	//
	private ByteCollections() { }
	
	//
	private static class _Empty extends AbstractByteCollection {

		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllByte(ByteCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllByte(
				Collection<? extends ByteCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsByte(byte v) {
			return false;
		}

		public boolean containsAllByte(ByteCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public ByteIterator byteIterator() {
			return new ByteIterator() {

				public boolean hasNext() {
					return false;
				}

				public byte next() {
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
		 * @see net.morilib.util.primitive.ByteCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final ByteSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractByteCollection {
		
		//
		/*package*/ ByteCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(ByteCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#add(byte)
		 */
		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection)
		 */
		public boolean addAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#addAll(net.morilib.util.primitive.ByteCollection[])
		 */
		public boolean addAllByte(ByteCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#addAll(java.util.Collection)
		 */
		public boolean addAllByte(
				Collection<? extends ByteCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#contains(byte)
		 */
		public boolean containsByte(byte v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#containsAll(net.morilib.util.primitive.ByteCollection)
		 */
		public boolean containsAllByte(ByteCollection a) {
			return wrapee.containsAllByte(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements ByteIterator {
			
			//
			private ByteIterator itr;
			
			//
			private Itr(ByteIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ByteIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ByteIterator#next()
			 */
			public byte next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.ByteIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#iterator()
		 */
		public ByteIterator byteIterator() {
			return new Itr(wrapee.byteIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#remove(byte)
		 */
		public boolean removeByte(byte v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#removeAll(net.morilib.util.primitive.ByteCollection)
		 */
		public boolean removeAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#retainAll(net.morilib.util.primitive.ByteCollection)
		 */
		public boolean retainAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#toArray()
		 */
		public byte[] toByteArray() {
			return wrapee.toByteArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#toArray(byte[])
		 */
		public byte[] toByteArray(byte[] a) {
			return wrapee.toByteArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteCollection#toSet()
		 */
		public ByteSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements ByteSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(ByteCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSet#collect(net.morilib.util.primitive.ByteSet)
		 */
		public ByteSet collect(ByteSet set) {
			return ((ByteSet)wrapee).collect(set);
		}
		
	}
	
	
	public static ByteCollection unmodifiableCollection(
			ByteCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	
	public static ByteSet unmodifiableSet(ByteSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements ByteSortedSet {

		public ByteSet collect(ByteSet set) {
			return this;
		}

		public ByteSortedSet collect(ByteSortedSet set) {
			return this;
		}

		public byte first() {
			throw new NoSuchElementException();
		}

		public ByteSortedSet headSet(byte v) {
			return this;
		}

		public ByteSortedSet subSet(byte s, byte e) {
			return this;
		}

		public ByteSortedSet tailSet(byte v) {
			return this;
		}

		public byte last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final ByteSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements ByteSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(ByteCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSet#collect(net.morilib.util.primitive.ByteSet)
		 */
		public ByteSortedSet collect(ByteSortedSet set) {
			return ((ByteSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSortedSet#first()
		 */
		public byte first() {
			return ((ByteSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSortedSet#headSet(byte)
		 */
		public ByteSortedSet headSet(byte v) {
			return unmodifiableSortedSet(
					((ByteSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSortedSet#subSet(byte, byte)
		 */
		public ByteSortedSet subSet(byte s, byte e) {
			return unmodifiableSortedSet(
					((ByteSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSortedSet#tailSet(byte)
		 */
		public ByteSortedSet tailSet(byte v) {
			return unmodifiableSortedSet(
					((ByteSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.ByteSortedSet#last()
		 */
		public byte last() {
			return ((ByteSortedSet)wrapee).last();
		}
		
	}
	
	
	public static ByteSortedSet unmodifiableSortedSet(
			ByteSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractByteVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public byte first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Byte> c) {
			throw new UnsupportedOperationException();
		}

		public Byte get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Byte set(int index, Byte element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Byte element) {
			throw new UnsupportedOperationException();
		}

		public Byte remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Byte> {

			public boolean hasNext() {
				return false;
			}

			public Byte next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Byte previous() {
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

			public void set(Byte e) {
				throw new UnsupportedOperationException();
			}

			public void add(Byte e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Byte> listIterator() {
			return ITR;
		}

		public ListIterator<Byte> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Byte> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addByte(int index, byte v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllByte(int index, ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public byte getByte(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfByte(byte v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public byte removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public ByteList rest() {
			throw new IndexOutOfBoundsException();
		}

		public ByteList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public byte setByte(int index, byte v) {
			throw new UnsupportedOperationException();
		}

		public byte set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfByte(byte v) {
			return -1;
		}

		public ByteVectorIterator byteVectorIterator() {
			return ByteIterators.NULL_ITERATOR;
		}

		public ByteVectorIterator byteVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return byteVectorIterator();
		}

		public ByteVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final ByteVector EMPTY_VECTOR = new _EmptyV();
	
	// %DELETE%
	//
	private static class Intv extends AbstractByteSet
	implements ByteSortedSet {
		
		//
		private byte f, t;
		
		//
		private Intv(byte f, byte t) {
			this.f = f;
			this.t = t;
		}

		public boolean addByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean removeByte(byte v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllByte(ByteCollection a) {
			throw new UnsupportedOperationException();
		}

		public ByteIterator byteIterator() {
			return new ByteIterator() {
				
				//
				private byte ptr = f;

				public boolean hasNext() {
					return ptr <= t;
				}

				public byte next() {
					return ptr++;
				}

				public void remove() {
					throw new UnsupportedOperationException();
				}
				
			};
		}

		public int size() {
			return t - f + 1;
		}

		public ByteSortedSet collect(ByteSortedSet set) {
			ByteSortedSet r = new ByteBitSet(this);
			
			r.removeAllByte(set);
			return ByteCollections.unmodifiableSortedSet(r);
		}

		public byte first() {
			return f;
		}

		public ByteSortedSet headSet(byte v) {
			if(v <= f) {
				return ByteCollections.EMPTY_SORTED_SET;
			} else if(v <= t) {
				return new Intv(f, --v);
			} else {
				return this;
			}
		}

		public ByteSortedSet subSet(byte s, byte e) {
			byte s2, e2;
			
			if(s > e) {
				throw new IllegalArgumentException();
			} else if(e == Byte.MIN_VALUE) {
				return ByteCollections.EMPTY_SORTED_SET;
			}
			
			e--;
			s2 = (s < f) ? f : s;
			e2 = (e > t) ? t : e;
			if(s > t || e < f) {
				return ByteCollections.EMPTY_SORTED_SET;
			} else {
				return new Intv(s2, e2);
			}
		}

		public ByteSortedSet tailSet(byte v) {
			if(v > t) {
				return ByteCollections.EMPTY_SORTED_SET;
			} else if(v > f) {
				return new Intv(v, t);
			} else {
				return this;
			}
		}

		public byte last() {
			return t;
		}
		
		
	}
	
	
	public static ByteSortedSet interval(byte f, byte t) {
		if(f > t) {
			throw new IllegalArgumentException();
		}
		return new Intv(f, t);
	}
	// %DELETE_END%
	
}
