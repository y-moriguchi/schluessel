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
package net.morilib.lisp.swing.list;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/21
 */
public class LispMutableJListModel extends AbstractLispJListModel {

	//
	private List<Datum> list;

	/**
	 * 
	 */
	public LispMutableJListModel() {
		list = new ArrayList<Datum>();
	}

	/**
	 * 
	 * @param d
	 */
	public LispMutableJListModel(Datum d) {
		ConsIterator itr = new ConsIterator(d);

		list = new ArrayList<Datum>();
		while(itr.hasNext()) {
			list.add(itr.next());
		}
	}

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#getSize()
	 */
	@Override
	public int getSize() {
		return list.size();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#getElementAt(int)
	 */
	@Override
	public Datum getElementAt(int index) {
		return list.get(index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#isEmpty()
	 */
	@Override
	public boolean isEmpty() {
		return list.isEmpty();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#contains(java.lang.Object)
	 */
	@Override
	public boolean contains(Object o) {
		return list.contains(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#iterator()
	 */
	@Override
	public Iterator<Datum> iterator() {
		final Iterator<Datum> itr = list.iterator();

		return new Iterator<Datum>() {

			@Override
			public boolean hasNext() {
				return itr.hasNext();
			}

			@Override
			public Datum next() {
				return itr.next();
			}

			@Override
			public void remove() {
				throw new UnsupportedOperationException();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#add(net.morilib.lisp.Datum)
	 */
	@Override
	public boolean add(Datum e) {
		int index = list.size();

		list.add(e);
		fireIntervalAdded(this, index, index);
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#remove(java.lang.Object)
	 */
	@Override
	public boolean remove(Object o) {
		int index = list.indexOf(o);

		if(index >= 0) {
			list.remove(index);
			fireIntervalRemoved(this, index, index);
			return true;
		} else {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#containsAll(java.util.Collection)
	 */
	@Override
	public boolean containsAll(Collection<?> c) {
		return list.containsAll(c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#addAll(java.util.Collection)
	 */
	@Override
	public boolean addAll(Collection<? extends Datum> c) {
		return addAll(list.size(), c);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#addAll(int, java.util.Collection)
	 */
	@Override
	public boolean addAll(int index, Collection<? extends Datum> c) {
		if(c.isEmpty()) {
			return false;
		} else {
			list.addAll(c);
			fireIntervalAdded(this, index, index + c.size());
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#removeAll(java.util.Collection)
	 */
	@Override
	public boolean removeAll(Collection<?> c) {
		if(c.isEmpty() || list.removeAll(c)) {
			return false;
		} else {
			fireContentsChanged(this, 0, list.size());
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#retainAll(java.util.Collection)
	 */
	@Override
	public boolean retainAll(Collection<?> c) {
		if(c.isEmpty() || list.retainAll(c)) {
			return false;
		} else {
			fireContentsChanged(this, 0, list.size());
			return true;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#clear()
	 */
	@Override
	public void clear() {
		int size = list.size();

		list.clear();
		fireIntervalRemoved(this, 0, size);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#set(int, net.morilib.lisp.Datum)
	 */
	@Override
	public Datum set(int index, Datum element) {
		Datum r = list.set(index, element);

		fireContentsChanged(this, index, index);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#add(int, net.morilib.lisp.Datum)
	 */
	@Override
	public void add(int index, Datum e) {
		list.add(e);
		fireIntervalAdded(this, index, index);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#remove(int)
	 */
	@Override
	public Datum remove(int index) {
		Datum r = list.remove(index);

		fireIntervalRemoved(this, index, index);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#indexOf(java.lang.Object)
	 */
	@Override
	public int indexOf(Object o) {
		return list.indexOf(o);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.list.AbstractLispJListModel#lastIndexOf(java.lang.Object)
	 */
	@Override
	public int lastIndexOf(Object o) {
		return list.lastIndexOf(o);
	}

}
