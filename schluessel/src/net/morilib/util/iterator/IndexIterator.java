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
package net.morilib.util.iterator;

import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Implementation of iterator of a collection which can refer by indexes.
 * <p>インデックスにより参照できるコレクションのIterator実装である.
 * 
 * @author MORIGUCHI, Yuichiro 2005/01/02
 */
public abstract class IndexIterator<E> implements ListIterator<E> {
	
	//
	private int begin = 0;
	private int end;
	private boolean goodState = false;
	
	/**
	 * the index indecaded by this iterator.
	 * <p>このIteratorが示しているインデックスである.
	 */
	protected int index;
	
	/**
	 * creates an iterator which iterate from <i>begin</i> to
	 * <i>end</i>.
	 * インデックス<i>begin</i>から<i>end</i>までの範囲を反復する
	 * Iteratorを生成する.
	 * 
	 * @param index  starting index from <i>begin</i>
	 * @param begin  the starting index of iteration
	 * @param end    the ending   index of iteration
	 */
	public IndexIterator(int index, int begin, int end) {
		this.begin = begin;
		this.end   = end;
		
		this.index = index;
	}
	
	/**
	 * creates an iterator which iterate from <i>begin</i> to
	 * <i>end</i>.
	 * インデックス<i>begin</i>から<i>end</i>までの範囲を反復する
	 * Iteratorを生成する.
	 * 
	 * @param begin  the starting index of iteration
	 * @param end    the ending   index of iteration
	 */
	public IndexIterator(int begin, int end) {
		this(0, begin, end);
	}
	
	/**
	 * gets an object indecated by the given index.
	 * インデックスにより示されたオブジェクトを得る.
	 * 
	 * @param index  the index
	 * @return  an object indecated by the given index
	 */
	protected abstract E get(int index);
	
	/**
	 * sets an object indecated by the given index.
	 * オブジェクトを指定されたインデックスにセットする.
	 * 
	 * @param index  the index
	 * @param o  an object to be set
	 */
	protected abstract void set(int index, E o);
	
	/**
	 * returns true if the given index is in the index range for iteration.
	 * インデックスがIteratorの対象範囲に存在するときtrueを得る.
	 * 
	 * @param index  the index
	 */
	protected boolean isInRange(int index) {
		//return index >= 0 && index + begin <= end;
		return index >= begin && index < end;
	}
	
	/*
	 * returns 
	 * 
	 * @param index
	 * @return
	 */
	/*protected int getOffsetedIndex(int ind) {
		return ind + begin;
	}*/
	
	/*
	 * 
	 * @return
	 */
	/*protected boolean isGoodState() {
		return goodState;
	}*/

	/**
	 * returns true if this iterator has a next element.
	 * <p>Iteratorが次の要素を持つときにtrueを得る.
	 * 
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return index + begin < end;
	}

	/**
	 * returns true if this iterator has a previous element.
	 * <p>Iteratorが前の要素を持つときにtrueを得る.
	 * 
	 * @see java.util.ListIterator#hasPrevious()
	 */
	public boolean hasPrevious() {
		return index > 0 || (!goodState && end - begin > 0);
	}
	
	/**
	 * gets the index pointed by this iterator and forwards the pointer of
	 * this.
	 * <p>Iteratorにより示されているインデックスを得て、ポインタを進める.
	 * 
	 * @see java.util.ListIterator#nextIndex()
	 */
	public int nextIndex() {
		goodState = true;
		return (!hasNext()) ? end - begin : index++;
	}

	/**
	 * gets the index pointed by this iterator and
	 * backwards the pointer of this.
	 * <p>Iteratorにより示されているインデックスを得て、
	 * ポインタを戻す.
	 * 
	 * @see java.util.ListIterator#previousIndex()
	 */
	public int previousIndex() {
		if(!hasPrevious()) {
			return -1;
		} else if(goodState) {
			return --index;
		} else {
			goodState = true;
			index = end - begin - 1;
			return index;
		}
	}

	/**
	 * removes the element which is pointed by this iterator.
	 * <p>Iteratorが指し示す要素を削除する.
	 * このメソッドはサポートされていない.
	 * 
	 * @see java.util.Iterator#remove()
	 * @throws UnsupportedOperationException
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

	/**
	 * gets the value pointed by this iterator and forwards the pointer of
	 * this.
	 * <p>Iteratorにより示されている値を得て、ポインタを進める.
	 * 
	 * @see java.util.Iterator#next()
	 */
	public E next() {
		if(!hasNext()) {
			throw new NoSuchElementException();
		}
		return get(nextIndex() + begin);
	}

	/**
	 * gets the value pointed by this iterator and
	 * backwards the pointer of this.
	 * <p>Iteratorにより示されている値を得て、
	 * ポインタを戻す.
	 * 
	 * @see java.util.ListIterator#previous()
	 */
	public E previous() {
		if(!hasPrevious()) {
			throw new NoSuchElementException();
		}
		return get(previousIndex() + begin);
	}

	/**
	 * adds an object to the index pointed by this iterator.
	 * <p>Iteratorが指し示すところにオブジェクトを追加する.
	 * このメソッドはサポートされていない.
	 * 
	 * @see java.util.ListIterator#add(java.lang.Object)
	 * @throws UnsupportedOperationException
	 */
	public void add(E o) {
		throw new UnsupportedOperationException();
	}

	/**
	 * sets an object to the index pointed by this iterator.
	 * <p>Iteratorが指し示すところの要素を
	 * 与えられたオブジェクトにセットする.
	 * 
	 * @see java.util.ListIterator#set(java.lang.Object)
	 */
	public void set(E o) {
		if(!goodState) {
			throw new IllegalStateException();
		} else {
			set(index, o);
		}
	}

}
