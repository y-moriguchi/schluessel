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
package net.morilib.lisp.util;

import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.WeakHashMap;

public class WeakValues<E> implements Iterable<E> {
	
	private static final Object ARU = new Object();
	
	//
	private Map<E, Object> col = new WeakHashMap<E, Object>();
	
	
	public Iterator<E> iterator() {
		return col.keySet().iterator();
	}
	
	
	public Collection<E> values() {
		return col.keySet();
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#add(java.lang.Object)
	 */
	public boolean add(E o) {
		return col.put(o, ARU) == null;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#clear()
	 */
	public void clear() {
		col.clear();
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractCollection#contains(java.lang.Object)
	 */
	public boolean contains(E o) {
		return col.get(o) == ARU;
	}

}
