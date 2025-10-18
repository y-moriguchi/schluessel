/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.util;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * <i>USEful Implements</i> for iterators.<br>
 * Iteratorに関する便利な関数である.
 * 
 * @author MORIGUCHI, Yuichiro 2005/01/01
 */
public final class Iterators {

	/**
	 * a empty list iterator.
	 * <p>空のListIteratorである.
	 */
	public static final ListIterator<Object> NULL =
		new ListIterator<Object>() {

		public void remove() {
			throw new UnsupportedOperationException();
		}

		public boolean hasNext() {
			return false;
		}

		public Object next() {
			throw new NoSuchElementException();
		}

		public int nextIndex() {
			return 0;
		}

		public int previousIndex() {
			return -1;
		}

		public boolean hasPrevious() {
			return false;
		}

		public Object previous() {
			throw new NoSuchElementException();
		}

		public void add(Object o) {
			throw new UnsupportedOperationException();
		}

		public void set(Object o) {
			throw new UnsupportedOperationException();
		}

	};

	//
	private Iterators() { }

	//
	private static class Unmodify<E> implements Iterator<E> {

		//
		Iterator<E> iter;

		//
		Unmodify(Iterator<E> iter) {
			this.iter = iter;
		}

		//
		public void remove() {
			throw new UnsupportedOperationException();
		}

		//
		public boolean hasNext() {
			return iter.hasNext();
		}

		//
		public E next() {
			return iter.next();
		}

	};

	//
	private static class Single<E> implements Iterator<E> {

		//
		private E obj;

		//
		Single(E o) {
			obj = o;
		}

		//
		public void remove() {
			throw new UnsupportedOperationException();
		}

		//
		public boolean hasNext() {
			return obj != null;
		}

		//
		public E next() {
			E o = obj;

			if(o == null) {
				throw new NoSuchElementException();
			}
			obj = null;
			return o;
		}

	};

	/**
	 * gets an empty iterator.
	 * <p>空のiteratorを得る。
	 * 
	 * @return an empty iterator
	 */
	@SuppressWarnings("unchecked")
	public static<E> ListIterator<E> emptyIterator() {
		return (ListIterator<E>)NULL;
	}

	/**
	 * gets the next non-null element of the given iterator.
	 * <p>引数のIteratorの次のnullでない要素を得る.
	 * 
	 * @param iter  the iterator
	 * @return  the next non-null element
	 */
	public static<E> E nextNotNull(Iterator<E> iter) {
		E res = null;

		do {
			if(!iter.hasNext()) {
				return null;
			}
			res = iter.next();
		} while(res == null);
		return res;
	}

	/**
	 * gets the unmodifiable iterator of the given iterator.
	 * <p>引数のIteratorを変更不可にしたものを得る.
	 * 
	 * @param i  the iterator
	 * @return  the unmodifiable iterator
	 */
	public static<E> Iterator<E> unmodifiable(Iterator<E> i) {
		return new Unmodify<E>(i);
	}

	/**
	 * gets the singleton iterator which has only the given object.
	 * <p>引数のオブジェクトのみ所有するIteratorを得る.
	 * 
	 * @param o  the object
	 * @return  the singleton iterator
	 */
	public static<E> Iterator<E> singleton(E o) {
		return new Single<E>(o);
	}

	/**
	 * returns the string representation of the given iterator.
	 * <p>引数のIteratorの文字列表現を得る.
	 * 
	 * @param i  the iterator
	 * @param delim  the delimiter which separates the elements
	 * @return  the string representation of the array
	 */
	public static<E> String toString(Iterator<E> i, String delim) {
		StringBuffer buf = new StringBuffer();
		String d2 = "";

		while(i.hasNext()) {
			buf.append(d2);
			buf.append(Objects.toString(i.next()));
			d2 = delim;
		}
		return buf.toString();		
	}

	/**
	 * 
	 * @param <E>
	 * @param i
	 */
	public static<E> E nextIf(Iterator<E> i) {
		return i.hasNext() ? i.next() : null;
	}

	/**
	 * 
	 * @param <E>
	 * @param i
	 */
	public static<E> E nextIf(Iterator<E> i, E ifnull) {
		return i.hasNext() ? i.next() : ifnull;
	}

	/**
	 * 
	 * @param <E>
	 * @param i
	 */
	public static<E> E nextIf(Iterator<E> i, RuntimeException ifnull) {
		if(i.hasNext()) {
			return i.next();
		} else {
			throw ifnull;
		}
	}

	/**
	 * @param iterator
	 * @return
	 */
	public static<E> List<E> toList(Iterator<E> iterator) {
		List<E> r = new ArrayList<E>();

		while(iterator.hasNext()) {
			r.add(iterator.next());
		}
		return r;
	}

	/**
	 * @param iterator
	 * @return
	 */
	public static<E> List<E> toList(Enumeration<E> iterator) {
		List<E> r = new ArrayList<E>();

		while(iterator.hasMoreElements()) {
			r.add(iterator.nextElement());
		}
		return r;
	}

	/**
	 * @param consIterator
	 * @return
	 */
	public static int length(Iterator<?> itr) {
		int l = 0;

		while(itr.hasNext()) {
			itr.next();
			l++;
		}
		return l;
	}

	/**
	 * 
	 * @param <T>
	 * @param iterator
	 * @param n
	 * @return
	 */
	public static<T> T get(Iterable<T> iterator, int n) {
		Iterator<T> itr = iterator.iterator();

		for(int i = 0; itr.hasNext() && i < n; itr.next(), i++);
		if(itr.hasNext()) {
			return itr.next();
		} else {
			throw new IndexOutOfBoundsException();
		}
	}

	/**
	 * 
	 * @param <T>
	 * @param iterator
	 * @param cmp
	 * @return
	 */
	public static<T extends Comparable<? super T>> boolean isSorted(
			Iterator<T> iterator) {
		T x1 = null, x2;

		while(iterator.hasNext()) {
			if(x1 == null) {
				x1 = iterator.next();
			} else if((x2 = iterator.next()) == null) {
				throw new NullPointerException();
			} else if(x1.compareTo(x2) > 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param <T>
	 * @param iterator
	 * @param cmp
	 * @return
	 */
	public static<T> boolean isSorted(Iterator<T> iterator,
			Comparator<? super T> cmp) {
		T x1 = null, x2;

		while(iterator.hasNext()) {
			if(x1 == null) {
				x1 = iterator.next();
			} else if((x2 = iterator.next()) == null) {
				throw new NullPointerException();
			} else if(cmp.compare(x1, x2) > 0) {
				return false;
			}
		}
		return true;
	}

	/**
	 * 
	 * @param <T>
	 * @param e
	 * @return
	 */
	public static<T> Iterator<T> toIterator(final Enumeration<T> e) {
		return new Iterator<T>() {

			public boolean hasNext() {
				return e.hasMoreElements();
			}

			public T next() {
				return e.nextElement();
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

}
