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
package net.morilib.util.bit;

import java.util.Collection;
import java.util.ConcurrentModificationException;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;
import java.util.NoSuchElementException;
import java.util.RandomAccess;

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/24
 */
public abstract class AbstractBitVector implements BitVector {

	//
	/*package*/ int modcount = 0;

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#add(boolean)
	 */
	public boolean add(boolean x) {
		return add(size(), x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#addAll(net.morilib.util.bit.BitCollection)
	 */
	public boolean addAllBoolean(BitCollection col) {
		return addAllBoolean(size(), col);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#clear()
	 */
	public void clear() {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				removeAt(i);
			}
		} else {
			BitIterator i = bitIterator();

			while(i.hasNext()) {
				i.next();
				i.remove();
			}
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#any(boolean)
	 */
	public boolean any(boolean x) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getBoolean(i) == x) {
					return true;
				}
			}
		} else {
			BitIterator i = bitIterator();

			while(i.hasNext()) {
				if(i.next() == x) {
					return true;
				}
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#every(boolean)
	 */
	public boolean every(boolean x) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getBoolean(i) != x) {
					return false;
				}
			}
		} else {
			BitIterator i = bitIterator();

			while(i.hasNext()) {
				if(i.next() != x) {
					return false;
				}
			}
		}
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}

	//
	private class Itr implements BitVectorIterator {

		//
		private int index;
		private int expectModcount;
		private boolean modified;

		//
		private Itr(int i) {
			index = i;
			expectModcount = modcount;
			modified = true;
		}

		//
		private Itr() {
			index = -10;
			expectModcount = modcount;
			modified = true;
		}

		//
		public boolean next() {
			if(index == -10) {
				index = 0;
			} else if(index >= size()) {
				throw new NoSuchElementException();
			} else {
				index++;
			}
			modified = false;
			return getBoolean(index);
		}

		//
		public boolean hasNext() {
			return (index == -10) ? size() > 0 : index + 1 < size();
		}

		//
		private void checkCon() {
			if(expectModcount != modcount) {
				throw new ConcurrentModificationException();
			}
		}

		//
		public void remove() {
			checkCon();
			if(modified) {
				throw new IllegalStateException();
			}
			removeAt(index--);
			expectModcount = modcount;
			modified = true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#add(boolean)
		 */
		public void add(boolean x) {
			checkCon();
			if(index == -10) {
				index = 0;
			}
			AbstractBitVector.this.add(index++, x);
			expectModcount = modcount;
			modified = true;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#hasPrevious()
		 */
		public boolean hasPrevious() {
			return (index == -10) ? size() > 0 : index > 0;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#nextIndex()
		 */
		public int nextIndex() {
			return (index == -10) ? 0 : index + 1;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#previous()
		 */
		public boolean previous() {
			if(index == -10) {
				index = size();
			} else if(index < 0) {
				throw new NoSuchElementException();
			}
			modified = false;
			return getBoolean(--index);
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#previousIndex()
		 */
		public int previousIndex() {
			return (index == -10) ? size() - 1 : index - 1;
		}

		/* (non-Javadoc)
		 * @see net.morilib.util.bit.BitVectorIterator#set(boolean)
		 */
		public void set(boolean x) {
			if(modified) {
				throw new IllegalStateException();
			}
			checkCon();
			AbstractBitVector.this.set(index, x);
			expectModcount = modcount;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#bitIterator()
	 */
	public BitIterator bitIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#toBooleanArray()
	 */
	public boolean[] toBooleanArray() {
		return toBooleanArray(new boolean[0]);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitCollection#toBooleanArray(boolean[])
	 */
	public boolean[] toBooleanArray(boolean[] arr) {
		boolean[] res;

		res = (arr.length < size()) ? new boolean[size()] : arr;
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				res[i] = getBoolean(i);
			}
		} else {
			BitIterator itr = bitIterator();

			for(int i = 0; itr.hasNext(); i++) {
				res[i] = itr.next();
			}
		}
		return res;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#add(int, boolean)
	 */
	public boolean add(int index, boolean x) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#addAll(int, net.morilib.util.bit.BitCollection)
	 */
	public boolean addAllBoolean(int index, BitCollection col) {
		boolean r = false;

		if(col instanceof BitVector && col instanceof RandomAccess) {
			for(int i = index; i < col.size(); i++) {
				add(i, ((BitVector)col).getBoolean(i));
				r = true;
			}
		} else {
			BitIterator itr = col.bitIterator();

			for(int i = index; itr.hasNext(); i++) {
				add(i, itr.next());
				r = true;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#indexOf(boolean)
	 */
	public int indexOf(boolean x) {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				if(getBoolean(i) == x) {
					return i;
				}
			}
		} else {
			BitIterator itr = bitIterator();

			for(int i = 0; itr.hasNext(); i++) {
				if(itr.next() == x) {
					return i;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#lastIndexOf(boolean)
	 */
	public int lastIndexOf(boolean x) {
		if(this instanceof RandomAccess) {
			for(int i = size() - 1; i >= 0; i--) {
				if(getBoolean(i) == x) {
					return i;
				}
			}
		} else {
			BitVectorIterator itr = bitVectorIterator();

			for(int i = size() - 1; itr.hasPrevious(); i--) {
				if(itr.previous() == x) {
					return i;
				}
			}
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#bitVectorIterator()
	 */
	public BitVectorIterator bitVectorIterator() {
		return new Itr();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#bitVectorIterator(int)
	 */
	public BitVectorIterator bitVectorIterator(int index) {
		if(index < 0 || index > size()) {
			throw new IndexOutOfBoundsException("" + index);
		}
		return new Itr(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#removeAt(int)
	 */
	public boolean removeAt(int index) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#set(int, boolean)
	 */
	public boolean set(int index, boolean x) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#subVector(int, int)
	 */
	public BitVector subVector(int start, int end) {
		final int s = start;
		final int e = end;

		if(s < 0 || s >= size()) {
			throw new IndexOutOfBoundsException("" + s);
		} else if(e < 0 || e > size()) {
			throw new IndexOutOfBoundsException("" + e);
		} else if(e < s) {
			throw new IllegalArgumentException();
		}

		return new AbstractBitVector() {

			@Override
			public boolean add(int index, boolean x) {
				if(index < 0 || index > e - s) {
					throw new IndexOutOfBoundsException("" + index);
				}
				return AbstractBitVector.this.add(s + index, x);
			}

			@Override
			public boolean removeAt(int index) {
				if(index < 0 || index >= e - s) {
					throw new IndexOutOfBoundsException("" + index);
				}
				return AbstractBitVector.this.removeAt(s + index);
			}

			@Override
			public boolean set(int index, boolean x) {
				if(index < 0 || index >= e - s) {
					throw new IndexOutOfBoundsException("" + index);
				}
				return AbstractBitVector.this.set(s + index, x);
			}

			public boolean getBoolean(int index) {
				if(index < 0 || index >= e - s) {
					throw new IndexOutOfBoundsException("" + index);
				}
				return AbstractBitVector.this.getBoolean(s + index);
			}

			public int size() {
				return e - s;
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#contains(java.lang.Object)
	 */
	public boolean contains(Object o) {
		if(o instanceof Boolean) {
			return any(((Boolean)o).booleanValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.List#iterator()
	 */
	public Iterator<Boolean> iterator() {
		final BitIterator b = bitIterator();

		return new Iterator<Boolean>() {

			public boolean hasNext() {
				return b.hasNext();
			}

			public Boolean next() {
				return b.next();
			}

			public void remove() {
				b.remove();
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#toArray()
	 */
	public Object[] toArray() {
		return toArray(new Boolean[0]);
	}

	/* (non-Javadoc)
	 * @see java.util.List#toArray(T[])
	 */
	@SuppressWarnings("unchecked")
	public <T> T[] toArray(T[] a) {
		Boolean[] res;

		res = (a.length < size()) ? new Boolean[size()] : (Boolean[])a;
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				res[i] = getBoolean(i);
			}
		} else {
			BitIterator itr = bitIterator();

			for(int i = 0; itr.hasNext(); i++) {
				res[i] = itr.next();
			}
		}
		return (T[])res;
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(java.lang.Object)
	 */
	public boolean add(Boolean e) {
		return add(e.booleanValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(java.lang.Object)
	 */
	public boolean remove(Object o) {
		if(o instanceof Boolean) {
			BitIterator itr = bitIterator();
			boolean r = false;

			while(itr.hasNext()) {
				if(itr.next() == ((Boolean)o).booleanValue()) {
					itr.remove();
					r = true;
				}
			}
			return r;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.List#containsAll(java.util.Collection)
	 */
	public boolean containsAll(Collection<?> c) {
		if(c.contains(Boolean.TRUE)) {
			return c.contains(Boolean.FALSE) ? !isEmpty() : any(true);
		} else {
			return c.contains(Boolean.FALSE) ? any(false) : false;
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#addAll(java.util.Collection)
	 */
	public boolean addAll(Collection<? extends Boolean> c) {
		return addAll(size(), c);
	}

	/* (non-Javadoc)
	 * @see java.util.List#addAll(int, java.util.Collection)
	 */
	public boolean addAll(int index, Collection<? extends Boolean> c) {
		boolean r = false;

		if(c instanceof RandomAccess) {
			for(int i = index; i < size(); i++) {
				Boolean x = ((List<? extends Boolean>)c).get(i);

				if(x == null) {
					throw new NullPointerException();
				}
				add(i, x.booleanValue());
				r = true;
			}
		} else {
			Iterator<? extends Boolean> itr = c.iterator();

			for(int i = index; itr.hasNext(); i++) {
				Boolean x = itr.next();

				if(x == null) {
					throw new NullPointerException();
				}
				add(i, x.booleanValue());
				r = true;
			}
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.List#removeAll(java.util.Collection)
	 */
	public boolean removeAll(Collection<?> c) {
		if(c.contains(Boolean.TRUE)) {
			if(c.contains(Boolean.FALSE)) {
				boolean r = isEmpty();

				clear();
				return !r;
			} else {
				return remove(Boolean.TRUE);
			}
		} else {
			if(c.contains(Boolean.FALSE)) {
				return remove(Boolean.FALSE);
			} else {
				return false;
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#retainAll(java.util.Collection)
	 */
	public boolean retainAll(Collection<?> c) {
		if(c.contains(Boolean.TRUE)) {
			if(c.contains(Boolean.FALSE)) {
				return false;
			} else {
				return remove(Boolean.FALSE);
			}
		} else {
			if(c.contains(Boolean.FALSE)) {
				return remove(Boolean.TRUE);
			} else {
				boolean r = isEmpty();

				clear();
				return !r;
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.util.List#get(int)
	 */
	public Boolean get(int index) {
		return getBoolean(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#set(int, java.lang.Object)
	 */
	public Boolean set(int index, Boolean element) {
		if(element == null) {
			throw new NullPointerException();
		}
		return set(index, element.booleanValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#add(int, java.lang.Object)
	 */
	public void add(int index, Boolean element) {
		if(element == null) {
			throw new NullPointerException();
		}
		add(index, element.booleanValue());
	}

	/* (non-Javadoc)
	 * @see java.util.List#remove(int)
	 */
	public Boolean remove(int index) {
		return removeAt(index);
	}

	/* (non-Javadoc)
	 * @see java.util.List#indexOf(java.lang.Object)
	 */
	public int indexOf(Object o) {
		if(o instanceof Boolean) {
			return indexOf(((Boolean)o).booleanValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#lastIndexOf(java.lang.Object)
	 */
	public int lastIndexOf(Object o) {
		if(o instanceof Boolean) {
			return lastIndexOf(((Boolean)o).booleanValue());
		}
		return -1;
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator()
	 */
	public ListIterator<Boolean> listIterator() {
		final BitVectorIterator b = bitVectorIterator();

		return new ListIterator<Boolean>() {

			public boolean hasNext() {
				return b.hasNext();
			}

			public Boolean next() {
				return b.next();
			}

			public boolean hasPrevious() {
				return b.hasPrevious();
			}

			public Boolean previous() {
				return b.previous();
			}

			public int nextIndex() {
				return b.nextIndex();
			}

			public int previousIndex() {
				return b.previousIndex();
			}

			public void remove() {
				b.remove();
			}

			public void set(Boolean e) {
				if(e == null) {
					throw new NullPointerException();
				}
				b.set(e.booleanValue());
			}

			public void add(Boolean e) {
				if(e == null) {
					throw new NullPointerException();
				}
				b.add(e.booleanValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#listIterator(int)
	 */
	public ListIterator<Boolean> listIterator(int index) {
		final BitVectorIterator b = bitVectorIterator(index);

		return new ListIterator<Boolean>() {

			public boolean hasNext() {
				return b.hasNext();
			}

			public Boolean next() {
				return b.next();
			}

			public boolean hasPrevious() {
				return b.hasPrevious();
			}

			public Boolean previous() {
				return b.previous();
			}

			public int nextIndex() {
				return b.nextIndex();
			}

			public int previousIndex() {
				return b.previousIndex();
			}

			public void remove() {
				b.remove();
			}

			public void set(Boolean e) {
				if(e == null) {
					throw new NullPointerException();
				}
				b.set(e.booleanValue());
			}

			public void add(Boolean e) {
				if(e == null) {
					throw new NullPointerException();
				}
				b.add(e.booleanValue());
			}

		};
	}

	/* (non-Javadoc)
	 * @see java.util.List#subList(int, int)
	 */
	public List<Boolean> subList(int fromIndex, int toIndex) {
		return subVector(fromIndex, toIndex);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.bit.BitVector#negate()
	 */
	public void negate() {
		if(this instanceof RandomAccess) {
			for(int i = 0; i < size(); i++) {
				set(i, !getBoolean(i));
			}
		} else {
			BitVectorIterator i = bitVectorIterator();

			while(i.hasNext()) {
				i.set(!i.next());
			}
		}
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof BitVector) {
			BitVector b = (BitVector)o;

			if(size() != b.size()) {
				return false;
			} else if(
					this instanceof RandomAccess &&
					o    instanceof RandomAccess) {
				for(int i = 0; i < size(); i++) {
					if(getBoolean(i) != b.getBoolean(i)) {
						return false;
					}
				}
			} else {
				BitVectorIterator i = bitVectorIterator();
				BitVectorIterator j = b.bitVectorIterator();

				while(i.hasNext()) {
					if(i.next() != j.next()) {
						return false;
					}
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;

		if(this instanceof RandomAccess) {
			for(int i = 0; i < size() && i < 32; i++) {
				r = getBoolean(i) ? r | (1 << i) : r & ~(1 << i);
			}
		} else {
			BitVectorIterator i = bitVectorIterator();

			for(int c = 0; i.hasNext() && c < 32; c++) {
				r = i.next() ? r | (1 << c) : r & ~(1 << c);
			}
		}
		return (Hashes.INIT * r + size()) * Hashes.A;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		BitVectorIterator i = bitVectorIterator();
		StringBuilder b = new StringBuilder();
		String d = "";

		b.append("[");
		while(i.hasNext()) {
			b.append(d);
			b.append(i.next());
			d = " ,";
		}
		b.append("]");
		return b.toString();
	}

}
