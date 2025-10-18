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
package net.morilib.util.string;

import java.util.AbstractMap;
import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;
import net.morilib.util.map.KeyTyped;
import net.morilib.util.primitive.CharacterArrayVector;
import net.morilib.util.primitive.CharacterVector;
import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.iterator.CharacterIterators;
import net.morilib.util.primitive.map.po.CharacterHashMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/07
 */
@KeyTyped(String.class)
public class StringTrieMap<V> extends AbstractMap<String, V> {
	
	//
	private Set<Map.Entry<String, V>> entrySet = null;
	private Entry<V> root = null;
	private int size;
	
	//
	/*package*/ static class Entry<V> {
		
		/*package*/ boolean exist;
		/*package*/ V value;
		/*package*/ CharacterMap<Entry<V>> entries = null;
		
	}
	
	//
	/*package*/ static class Iter<V>
	implements Iterator<Map.Entry<String, V>> {
		
		//
		private Entry<V> now;
		private
		Stack2<Iterator<CharacterMap.PrimitiveEntryK<Entry<V>>>>
		iteratorStack =
			new ArrayListStack
			<Iterator<CharacterMap.PrimitiveEntryK<Entry<V>>>>();
		private CharacterVector res = new CharacterArrayVector();
		
		//
		private boolean _ready() {
			while(!iteratorStack.isEmpty() &&
					!iteratorStack.peek().hasNext()) {
				iteratorStack.pop();
				res.remove(res.size() - 1);
			}
			return iteratorStack.isEmpty();
		}
		
		//
		private Entry<V> _next() {
			while(true) {
				CharacterMap.PrimitiveEntryK<Entry<V>> e;
				
				if(now.entries != null) {
					iteratorStack.push(
							now.entries.charKeyEntrySet().iterator());
				} else if(iteratorStack.peek().hasNext()) {
					res.remove(res.size() - 1);
				} else if(_ready()) {
					return null;
				}
				
				e = iteratorStack.peek().next();
				res.addChar(e.getKey());
				if(e.getValue().exist) {
					return e.getValue();
				}
			}
			
		}
		
		/*package*/ Iter(Entry<V> now) {
			this.now = now;
			if(now != null) {
				iteratorStack.push(
						now.entries.charKeyEntrySet().iterator());
			}
		}
		
		/* (non-Javadoc)
		 * @see java.util.Iterator#hasNext()
		 */
		public boolean hasNext() {
			return iteratorStack.isEmpty();
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#next()
		 */
		public Map.Entry<String, V> next() {
			if(!hasNext()) {
				throw new NoSuchElementException();
			}
			
			final Entry<V> e = now;
			final String   s = new String(res.toCharArray());
			now = _next();
			return new Map.Entry<String, V>() {

				public String getKey() {
					return s;
				}

				public V getValue() {
					return e.value;
				}

				public V setValue(V value) {
					V r = e.value;
					
					e.value = value;
					return r;
				}
				
			};
		}

		/* (non-Javadoc)
		 * @see java.util.Iterator#remove()
		 */
		public void remove() {
			throw new UnsupportedOperationException();
		}
		
	}
	
	//
	private class ESet extends AbstractSet<Map.Entry<String, V>> {

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#iterator()
		 */
		@Override
		public Iterator<Map.Entry<String, V>> iterator() {
			return new Iter<V>(root);
		}

		/* (non-Javadoc)
		 * @see java.util.AbstractCollection#size()
		 */
		@Override
		public int size() {
			return StringTrieMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see java.util.AbstractMap#entrySet()
	 */
	@Override
	public Set<Map.Entry<String, V>> entrySet() {
		if(entrySet == null) {
			entrySet = new ESet();
		}
		return entrySet();
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractMap#size()
	 */
	@Override
	public int size() {
		return size;
	}
	
	//
	private Entry<V> traverse(CharacterIterator s) {
		Entry<V> now = root;
		
		while(s.hasNext()) {
			char c = s.next();
			
			if(now.entries == null) {
				return null;
			} else if(!now.entries.containsKeyElement(c)) {
				return null;
			}
			now = now.entries.getElement(c);
		}
		return now;
	}
	
	/* (non-Javadoc)
	 * @see java.util.AbstractMap#containsKey(java.lang.Object)
	 */
	@Override
	public boolean containsKey(Object key) {
		if(key instanceof String) {
			Entry<V> e =
				traverse(CharacterIterators.newIterator((String)key));
			
			return e != null;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractMap#get(java.lang.Object)
	 */
	@Override
	public V get(Object key) {
		if(key instanceof String) {
			Entry<V> e =
				traverse(CharacterIterators.newIterator((String)key));
			
			return e.value;
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractMap#put(java.lang.Object, java.lang.Object)
	 */
	@Override
	public V put(String key, V value) {
		Entry<V> now = root;
		V r;
		
		CharacterIterator s = CharacterIterators.newIterator(key);
		while(s.hasNext()) {
			char c = s.next();
			
			if(now.entries == null) {
				now.entries = new CharacterHashMap<Entry<V>>();
				now.entries.putElement(c, new Entry<V>());
			} else if(!now.entries.containsKeyElement(c)) {
				now.entries.putElement(c, new Entry<V>());
			}
			now = now.entries.getElement(c);
		}
		r = now.value;
		now.exist = true;
		now.value = value;
		return r;
	}

	/* (non-Javadoc)
	 * @see java.util.AbstractMap#clear()
	 */
	@Override
	public void clear() {
		size = 0;
		root = null;
	}
	
}
