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

import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.iterator.CharacterIterators;
import net.morilib.util.primitive.iterator.CharacterVectorIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/16
 */
public final class CharacterCollections {
	
	//
	private CharacterCollections() { }
	
	//
	private static class _Empty extends AbstractCharacterCollection {

		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllChar(CharacterCollection... as) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllChar(
				Collection<? extends CharacterCollection> as) {
			throw new UnsupportedOperationException();
		}

		public void clear() {
			throw new UnsupportedOperationException();
		}

		public boolean containsChar(char v) {
			return false;
		}

		public boolean containsAllChar(CharacterCollection a) {
			return a.isEmpty();
		}

		public boolean isEmpty() {
			return true;
		}

		public boolean removeChar(char v) {
			throw new UnsupportedOperationException();
		}

		public boolean removeAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		public boolean retainAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		public CharacterIterator charIterator() {
			return new CharacterIterator() {

				public boolean hasNext() {
					return false;
				}

				public char next() {
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
		 * @see net.morilib.util.primitive.CharacterCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return true;
		}
		
	}
	
	/**
	 * 
	 */
	public static final CharacterSet EMPTY_SET = new _EmptyS();
	
	//
	/*package*/ static class UnmodifiableCol
	extends AbstractCharacterCollection {
		
		//
		/*package*/ CharacterCollection wrapee;
		
		//
		/*package*/ UnmodifiableCol(CharacterCollection s) {
			wrapee = s;
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#add(char)
		 */
		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection)
		 */
		public boolean addAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#addAll(net.morilib.util.primitive.CharacterCollection[])
		 */
		public boolean addAllChar(CharacterCollection... as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#addAll(java.util.Collection)
		 */
		public boolean addAllChar(
				Collection<? extends CharacterCollection> as) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#clear()
		 */
		public void clear() {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#contains(char)
		 */
		public boolean containsChar(char v) {
			return wrapee.contains(v);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#containsAll(net.morilib.util.primitive.CharacterCollection)
		 */
		public boolean containsAllChar(CharacterCollection a) {
			return wrapee.containsAllChar(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#isEmpty()
		 */
		public boolean isEmpty() {
			return wrapee.isEmpty();
		}
		
		//
		private static class Itr implements CharacterIterator {
			
			//
			private CharacterIterator itr;
			
			//
			private Itr(CharacterIterator itr) {
				this.itr = itr;
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.CharacterIterator#hasNext()
			 */
			public boolean hasNext() {
				return itr.hasNext();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.CharacterIterator#next()
			 */
			public char next() {
				return itr.next();
			}

			/* (non-Javadoc)
			 * @see net.morilib.util.primitive.iterator.CharacterIterator#remove()
			 */
			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#iterator()
		 */
		public CharacterIterator charIterator() {
			return new Itr(wrapee.charIterator());
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#remove(char)
		 */
		public boolean removeChar(char v) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#removeAll(net.morilib.util.primitive.CharacterCollection)
		 */
		public boolean removeAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#retainAll(net.morilib.util.primitive.CharacterCollection)
		 */
		public boolean retainAllChar(CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#size()
		 */
		public int size() {
			return wrapee.size();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#toArray()
		 */
		public char[] toCharArray() {
			return wrapee.toCharArray();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#toArray(char[])
		 */
		public char[] toCharArray(char[] a) {
			return wrapee.toCharArray(a);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#isInfinite()
		 */
		public boolean isInfinite() {
			return wrapee.isInfinite();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterCollection#toSet()
		 */
		public CharacterSet toSet() {
			return wrapee.toSet();
		}
		
	}
	
	//
	/*package*/ static class UnmodifiableSet
	extends UnmodifiableCol implements CharacterSet {

		/**
		 * @param s
		 */
		UnmodifiableSet(CharacterCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSet#collect(net.morilib.util.primitive.CharacterSet)
		 */
		public CharacterSet collect(CharacterSet set) {
			return ((CharacterSet)wrapee).collect(set);
		}
		
	}
	
	public static CharacterCollection unmodifiableCollection(
			CharacterCollection c) {
		return (c instanceof UnmodifiableCol) ?
				c : new UnmodifiableCol(c);
	}
	
	public static CharacterSet unmodifiableSet(CharacterSet set) {
		return (set instanceof UnmodifiableSet) ?
				set : new UnmodifiableSet(set);
	}
	
	//
	private static class _EmptyS extends _Empty
	implements CharacterSortedSet {

		public CharacterSet collect(CharacterSet set) {
			return this;
		}

		public CharacterSortedSet collect(CharacterSortedSet set) {
			return this;
		}

		public char first() {
			throw new NoSuchElementException();
		}

		public CharacterSortedSet headSet(char v) {
			return this;
		}

		public CharacterSortedSet subSet(char s, char e) {
			return this;
		}

		public CharacterSortedSet tailSet(char v) {
			return this;
		}

		public char last() {
			throw new NoSuchElementException();
		}
		
	}
	
	/**
	 * 
	 */
	public static final CharacterSortedSet EMPTY_SORTED_SET = new _EmptyS();
	
	/*package*/ static class UnmodifiableSSet
	extends UnmodifiableSet implements CharacterSortedSet {

		/**
		 * @param s
		 */
		UnmodifiableSSet(CharacterCollection s) {
			super(s);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSet#collect(net.morilib.util.primitive.CharacterSet)
		 */
		public CharacterSortedSet collect(CharacterSortedSet set) {
			return ((CharacterSortedSet)wrapee).collect(set);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSortedSet#first()
		 */
		public char first() {
			return ((CharacterSortedSet)wrapee).first();
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSortedSet#headSet(char)
		 */
		public CharacterSortedSet headSet(char v) {
			return unmodifiableSortedSet(
					((CharacterSortedSet)wrapee).headSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSortedSet#subSet(char, char)
		 */
		public CharacterSortedSet subSet(char s, char e) {
			return unmodifiableSortedSet(
					((CharacterSortedSet)wrapee).subSet(s, e));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSortedSet#tailSet(char)
		 */
		public CharacterSortedSet tailSet(char v) {
			return unmodifiableSortedSet(
					((CharacterSortedSet)wrapee).tailSet(v));
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.primitive.CharacterSortedSet#last()
		 */
		public char last() {
			return ((CharacterSortedSet)wrapee).last();
		}
		
	}
	
	public static CharacterSortedSet unmodifiableSortedSet(
			CharacterSortedSet set) {
		return (set instanceof UnmodifiableSSet) ?
				set : new UnmodifiableSSet(set);
	}
	
	//
	private static class _EmptyV extends AbstractCharacterVector
	implements java.io.Serializable {

		//
		private static final long serialVersionUID = 1540238742681016912L;

		public int size() {
			return 0;
		}

		public char first() {
			throw new NoSuchElementException();
		}

		public boolean addAll(
				int index, Collection<? extends Character> c) {
			throw new UnsupportedOperationException();
		}

		public Character get(int index) {
			throw new IndexOutOfBoundsException();
		}

		public Character set(int index, Character element) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, Character element) {
			throw new UnsupportedOperationException();
		}

		public Character remove(int index) {
			throw new UnsupportedOperationException();
		}

		public int indexOf(Object o) {
			return -1;
		}

		public int lastIndexOf(Object o) {
			return -1;
		}
		
		//
		private static class _Itr implements ListIterator<Character> {

			public boolean hasNext() {
				return false;
			}

			public Character next() {
				throw new NoSuchElementException();
			}

			public boolean hasPrevious() {
				return false;
			}

			public Character previous() {
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

			public void set(Character e) {
				throw new UnsupportedOperationException();
			}

			public void add(Character e) {
				throw new UnsupportedOperationException();
			}
			
		}
		
		//
		private static final _Itr ITR = new _Itr();
		
		public ListIterator<Character> listIterator() {
			return ITR;
		}

		public ListIterator<Character> listIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return listIterator();
		}

		public List<Character> subList(int fromIndex, int toIndex) {
			if(fromIndex != 0 || toIndex != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public void addChar(int index, char v) {
			throw new UnsupportedOperationException();
		}

		public void add(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public boolean addAllChar(int index, CharacterCollection a) {
			throw new UnsupportedOperationException();
		}

		public char getChar(int index) {
			throw new IndexOutOfBoundsException();
		}

		public int indexOfChar(char v) {
			return -1;
		}

		public int indexOf(int v) {
			return -1;
		}

		public char removeAt(int index) {
			throw new UnsupportedOperationException();
		}

		public CharacterList rest() {
			throw new IndexOutOfBoundsException();
		}

		public CharacterList rest(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}

		public char setChar(int index, char v) {
			throw new UnsupportedOperationException();
		}

		public char set(int index, int v) {
			throw new UnsupportedOperationException();
		}

		public int lastIndexOfChar(char v) {
			return -1;
		}

		public CharacterVectorIterator charVectorIterator() {
			return CharacterIterators.NULL_ITERATOR;
		}

		public CharacterVectorIterator charVectorIterator(int index) {
			if(index != 0) {
				throw new IndexOutOfBoundsException();
			}
			return charVectorIterator();
		}

		public CharacterVector subVector(int start, int end) {
			if(start != 0 || end != 0) {
				throw new IndexOutOfBoundsException();
			}
			return this;
		}
		
	}
	
	/**
	 * 
	 */
	public static final CharacterVector EMPTY_VECTOR = new _EmptyV();
	
}
