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

import java.util.Iterator;

import net.morilib.util.SimpleMap;

public class MappedIterator<E, V> implements Iterator<V> {
	
	//
	protected Iterator<E> iterator;
	protected SimpleMap<E, V> map;
	
	
	public MappedIterator(Iterator<E> iter, SimpleMap<E, V> map) {
		this.iterator = iter;
		this.map      = map;
	}

	
	public boolean hasNext() {
		return iterator.hasNext();
	}

	
	public V next() {
		return map.map(iterator.next());
	}

	
	public void remove() {
		iterator.remove();
	}

}
