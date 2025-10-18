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
package net.morilib.util.set;

import java.util.AbstractSet;
import java.util.Collection;
import java.util.Comparator;
import java.util.Iterator;
import java.util.SortedSet;

import net.morilib.lang.Hashes;
import net.morilib.lang.number.Numbers;

public class IntegerRangeSet extends AbstractSet<Integer>
implements SortedSet<Integer>, java.io.Serializable {
	
	//
	private static final long serialVersionUID = -6403675898652617915L;
	
	//
	private static final Comparator<? super Integer>
	_CMP = new Comparator<Integer>() {

		public int compare(Integer o1, Integer o2) {
			return (o1 > o2) ? 1 : (o1 < o2) ? -1 : 0;
		}
		
	};
	
	//
	private int from, to;
	
	//
	private class _Itr implements Iterator<Integer> {
		
		//
		private int now;
		
		private _Itr() {
			now = from;
		}
		
		public boolean hasNext() {
			return now <= to;
		}

		public Integer next() {
			return now++;
		}

		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
	
	
	public IntegerRangeSet(int from, int to) {
		if(from > to) {
			throw new IllegalArgumentException();
		}
		this.from = from;
		this.to   = to;
	}
	
	
	public boolean add(Integer o) {
		throw new UnsupportedOperationException();
	}

	public boolean addAll(Collection<? extends Integer> c) {
		throw new UnsupportedOperationException();
	}

	public void clear() {
		throw new UnsupportedOperationException();
	}

	public boolean contains(Object o) {
		if(o instanceof Number) {
			return Numbers.between((Number)o, from, to);
		}
		return false;
	}

	public boolean isEmpty() {
		return false;
	}

	public Iterator<Integer> iterator() {
		return new _Itr();
	}

	public boolean remove(Object o) {
		throw new UnsupportedOperationException();
	}

	public boolean removeAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public boolean retainAll(Collection<?> c) {
		throw new UnsupportedOperationException();
	}

	public int size() {
		return to - from + 1;
	}


	public Comparator<? super Integer> comparator() {
		return _CMP;
	}


	public Integer first() {
		return from;
	}


	public SortedSet<Integer> headSet(Integer toElement) {
		if(toElement < from || toElement > to) {
			throw new IllegalArgumentException();
		}
		return new IntegerRangeSet(from, toElement);
	}


	public Integer last() {
		return to;
	}


	public SortedSet<Integer> subSet(
			Integer fromElement, Integer toElement) {
		if(toElement < from || toElement > to) {
			throw new IllegalArgumentException();
		} else if(fromElement < from || fromElement > to) {
			throw new IllegalArgumentException();
		} else if(toElement < fromElement) {
			throw new IllegalArgumentException();
		}
		return new IntegerRangeSet(fromElement, toElement);
	}


	public SortedSet<Integer> tailSet(Integer fromElement) {
		if(fromElement < from || fromElement > to) {
			throw new IllegalArgumentException();
		}
		return new IntegerRangeSet(fromElement, to);
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractSet#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof IntegerRangeSet) {
			IntegerRangeSet s = (IntegerRangeSet)o;
			
			return from == s.from && to == s.to;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractSet#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;
		
		r = r * Hashes.A + from;
		r = r * Hashes.A + to;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#toString()
	 */
	@Override
	public String toString() {
		return "Z[" + from + ", " + to + "]";
	}

}
