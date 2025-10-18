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
package net.morilib.util.primitive.map.po;

import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import net.morilib.util.primitive.AbstractCharacterSet;
import net.morilib.util.primitive.CharacterSet;
import net.morilib.util.primitive.iterator.CharacterIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractCharacterMap<V> implements CharacterMap<V> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient CharacterSet keySet = null;
	private transient Collection<V> values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = charKeyEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#inRange(char)
	 */
	public boolean containsValue(Object v) {
		for(PrimitiveEntryK<V> e : charKeyEntrySet()) {
			Object p = e.getValue();
			
			if((v == null && p == null) ||
					(v != null && v.equals(p))) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#get(java.lang.Object)
	 */
	public V getElement(char k) {
		for(PrimitiveEntryK<V> e : charKeyEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractCharacterSet {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private KSet(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}
		
		public CharacterIterator charIterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new CharacterIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public char next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractCharacterMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterMap.this.clear();
		}

		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#keySet()
	 */
	public CharacterSet charKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(charKeyEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#putAll(net.morilib.util.primitive.map.op.ToCharacterMap)
	 */
	public void putAllElement(CharacterMap<V> map) {
		for(PrimitiveEntryK<V> e : map.charKeyEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractCollection<V> {
		
		//
		private Set<PrimitiveEntryK<V>> eset;
		
		private VCol(Set<PrimitiveEntryK<V>> eset) {
			this.eset = eset;
		}

		public Iterator<V> iterator() {
			final Iterator<PrimitiveEntryK<V>> ei = eset.iterator();
			
			return new Iterator<V>() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public V next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractCharacterMap.this.size();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#values()
	 */
	public Collection<V> values() {
		return (values == null) ?
				(values = new VCol(charKeyEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Character) {
			return containsKeyElement(((Character)key).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public V get(Object key) {
		if(key instanceof Character) {
			return getElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public V put(Character key, V value) {
		return putElement(key.charValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public V remove(Object key) {
		if(key instanceof Character) {
			return removeElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Character, ? extends V> m) {
		for(Map.Entry<? extends Character, ? extends V> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public Set<Character> keySet() {
		return charKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Character, V>> entrySet() {
		final Set<PrimitiveEntryK<V>> e = charKeyEntrySet();
		
		return new AbstractSet<Entry<Character, V>>() {

			public Iterator<Map.Entry<Character, V>> iterator() {
				final Iterator<PrimitiveEntryK<V>> i = e.iterator();
				
				return new Iterator<Entry<Character, V>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Map.Entry<Character, V> next() {
						final PrimitiveEntryK<V> o = i.next();
						
						return new Map.Entry<Character, V>() {

							public Character getKey() {
								return o.getKey();
							}

							public V getValue() {
								return o.getValue();
							}

							public V setValue(V value) {
								return o.setValue(value);
							}
							
						};
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return e.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#containsKeyChar(char)
	 */
	public boolean containsKeyElement(char k) {
		for(PrimitiveEntryK<V> e : charKeyEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#get(int)
	 */
	public V get(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#put(int, java.lang.Object)
	 */
	public V put(int k, V v) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((char)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#removeElement(char)
	 */
	public V removeElement(char k) {
		Iterator<PrimitiveEntryK<V>> i;
		
		i = charKeyEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntryK<V> e = i.next();
			
			if(e.getKey() == k) {
				V r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#remove(int)
	 */
	public V remove(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((char)k);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryK<V> e : charKeyEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof CharacterMap) {
			CharacterMap<V> m = (CharacterMap<V>)obj;
			Iterator<PrimitiveEntryK<V>> i;
			
			i = charKeyEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntryK<V> o = i.next();
				V v = m.getElement(o.getKey());
				V p = o.getValue();
				
				if(!m.containsKeyElement(o.getKey()) ||
						!((v == null && p == null) ||
								(v != null && v.equals(p)))) {
					return false;
				}
			}
			return true;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder b = new StringBuilder();
		String d = "";
		
		b.append("{");
		for(PrimitiveEntryK<V> e : charKeyEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
