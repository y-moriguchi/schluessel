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

import javax.swing.ListModel;
import javax.swing.event.ListDataEvent;
import javax.swing.event.ListDataListener;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.ILispVector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/21
 */
public abstract class AbstractLispJListModel extends Datum2
implements ListModel, ILispVector {

	//
	private List<ListDataListener> listeners =
			new ArrayList<ListDataListener>();

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#getElementAt(int)
	 */
	@Override
	public abstract Datum getElementAt(int index);

	/**
	 * 
	 * @return
	 */
	public Datum toList() {
		ConsListBuilder b = new ConsListBuilder();

		for(int i = 0; i < getSize(); i++) {
			b.append(getElementAt(i));
		}
		return b.get();
	}

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#getSize()
	 */
	@Override
	public int size() {
		return getSize();
	}

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#getElementAt(int)
	 */
	@Override
	public Datum get(int index) {
		return getElementAt(index);
	}

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#addListDataListener(javax.swing.event.ListDataListener)
	 */
	@Override
	public void addListDataListener(ListDataListener l) {
		listeners.add(l);
	}

	/* (non-Javadoc)
	 * @see javax.swing.ListModel#removeListDataListener(javax.swing.event.ListDataListener)
	 */
	@Override
	public void removeListDataListener(ListDataListener l) {
		listeners.remove(l);
	}

	/**
	 * 
	 * @param source
	 * @param index0
	 * @param index1
	 */
	protected void fireContentsChanged(Object source, int index0,
			int index1) {
		ListDataEvent e = null;

		for(ListDataListener l : listeners) {
			if(e == null) {
				e = new ListDataEvent(source,
						ListDataEvent.CONTENTS_CHANGED,
						index0, index1);
			}
			l.contentsChanged(e);
		}
	}

	/**
	 * 
	 * @param source
	 * @param index0
	 * @param index1
	 */
	protected void fireIntervalAdded(Object source, int index0,
			int index1) {
		ListDataEvent e = null;

		for(ListDataListener l : listeners) {
			if(e == null) {
				e = new ListDataEvent(source,
						ListDataEvent.INTERVAL_ADDED,
						index0, index1);
			}
			l.intervalAdded(e);
		}
	}

	/**
	 * 
	 * @param source
	 * @param index0
	 * @param index1
	 */
	protected void fireIntervalRemoved(Object source, int index0,
			int index1) {
		ListDataEvent e = null;

		for(ListDataListener l : listeners) {
			if(e == null) {
				e = new ListDataEvent(source,
						ListDataEvent.INTERVAL_REMOVED,
						index0, index1);
			}
			l.intervalRemoved(e);
		}
	}

	/**
	 * 
	 * @return
	 */
	public abstract boolean isEmpty();

	/**
	 * 
	 * @param o
	 * @return
	 */
	public abstract boolean contains(Object o);

	/**
	 * 
	 * @return
	 */
	public abstract Iterator<Datum> iterator();

	/**
	 * 
	 * @param e
	 * @return
	 */
	public abstract boolean add(Datum e);

	/**
	 * 
	 * @param o
	 * @return
	 */
	public abstract boolean remove(Object o);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract boolean containsAll(Collection<?> c);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract boolean addAll(Collection<? extends Datum> c);

	/**
	 * 
	 * @param index
	 * @param c
	 * @return
	 */
	public abstract boolean addAll(int index,
			Collection<? extends Datum> c);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract boolean removeAll(Collection<?> c);

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract boolean retainAll(Collection<?> c);

	/**
	 * 
	 */
	public abstract void clear();

	/**
	 * 
	 * @param index
	 * @param element
	 * @return
	 */
	public abstract Datum set(int index, Datum element);

	/**
	 * 
	 * @param index
	 * @param e
	 */
	public abstract void add(int index, Datum e);

	/**
	 * 
	 * @param index
	 * @return
	 */
	public abstract Datum remove(int index);

	/**
	 * 
	 * @param o
	 * @return
	 */
	public abstract int indexOf(Object o);

	/**
	 * 
	 * @param o
	 * @return
	 */
	public abstract int lastIndexOf(Object o);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<jlist-model>");
	}

}
