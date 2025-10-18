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
package net.morilib.util.primitive.map.op;

import java.util.AbstractCollection;
import java.util.AbstractSet;
import java.util.Collection;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.primitive.AbstractCharacterCollection;
import net.morilib.util.primitive.CharacterCollection;
import net.morilib.util.primitive.iterator.CharacterIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractCharacterValueMap<K> implements CharacterValueMap<K> {
	
	//
//	private static final long serialVersionUID = 4823620077843167065L;
	
	//
	private transient Set<K> keySet = null;
	private transient CharacterCollection values = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntryV<K>> i = charValueEntrySet().iterator();
		
		while(i.hasNext()) {
			i.remove();
		}
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#inDomain(java.lang.Object)
	 */
	public boolean containsKey(Object k) {
		if(k instanceof Character) {
			containsValueElement(((Character)k).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#inRange(char)
	 */
	public boolean containsValueElement(char v) {
		for(PrimitiveEntryV<K> e : charValueEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#get(java.lang.Object)
	 */
	public Character get(Object k) {
		if(k instanceof Character) {
			for(PrimitiveEntryV<K> e : charValueEntrySet()) {
				Object l = e.getKey();
				
				if((k == null && l == null) ||
						(k != null && k.equals(l))) {
					return e.getValue();
				}
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#getValue(char)
	 */
	public char getElement(Object k) {
		if(k instanceof Character) {
			for(PrimitiveEntryV<K> e : charValueEntrySet()) {
				Object l = e.getKey();
				
				if((k == null && l == null) ||
						(k != null && k.equals(l))) {
					return e.getValue();
				}
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractSet<K> {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private KSet(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}
		
		public Iterator<K> iterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
			return new Iterator<K>() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public K next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractCharacterValueMap.this.size();
		}

		public boolean contains(Object o) {
			return containsKey(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterValueMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterValueMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#keySet()
	 */
	public Set<K> keySet() {
		return (keySet == null) ?
				(keySet = new KSet(charValueEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#put(java.lang.Object, char)
	 */
	public Character put(K k, char v) {
		boolean b = containsKey(k);
		char r = putElement(k, v);
		
		return b ? r : null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#putAll(net.morilib.util.primitive.map.op.ToCharacterMap)
	 */
	public void putAllElement(CharacterValueMap<K> map) {
		for(PrimitiveEntryV<K> e : map.charValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractCharacterCollection {
		
		//
		private Set<PrimitiveEntryV<K>> eset;
		
		private VCol(Set<PrimitiveEntryV<K>> eset) {
			this.eset = eset;
		}

		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}

		public CharacterIterator charIterator() {
			final Iterator<PrimitiveEntryV<K>> ei = eset.iterator();
			
			return new CharacterIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public char next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractCharacterValueMap.this.size();
		}

		public boolean isInfinite() {
			return false;
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#values()
	 */
	public CharacterCollection charValues() {
		return (values == null) ?
				(values = new VCol(charValueEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((char)v);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Character) {
			return containsValueElement(
					((Character)value).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Character put(K key, Character value) {
		if(value instanceof Character) {
			return putElement(key, ((Character)value).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends K, ? extends Character> m) {
		for(PrimitiveEntryV<K> e : charValueEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#values()
	 */
	public Collection<Character> values() {
		final Iterator<PrimitiveEntryV<K>> i = charValueEntrySet().iterator();
		
		return new AbstractCollection<Character>() {

			public Iterator<Character> iterator() {
				return new Iterator<Character>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Character next() {
						return i.next().getValue();
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractCharacterValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<K, Character>> entrySet() {
		final Iterator<PrimitiveEntryV<K>> i = charValueEntrySet().iterator();
		
		return new AbstractSet<Entry<K, Character>>() {

			public Iterator<Entry<K, Character>> iterator() {
				return new Iterator<Entry<K, Character>>() {

					public boolean hasNext() {
						return i.hasNext();
					}

					public Entry<K, Character> next() {
						final PrimitiveEntryV<K> e = i.next();
						
						return new Map.Entry<K, Character>() {

							public K getKey() {
								return e.getKey();
							}

							public Character getValue() {
								return e.getValue();
							}

							public Character setValue(Character value) {
								return e.setValue(value);
							}
							
						};
					}

					public void remove() {
						i.remove();
					}
					
				};
			}

			public int size() {
				return AbstractCharacterValueMap.this.size();
			}
			
		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#remove(java.lang.Object)
	 */
	public Character remove(Object k) {
		Iterator<PrimitiveEntryV<K>> i = charValueEntrySet().iterator();
		
		while(i.hasNext()) {
			PrimitiveEntryV<K> e = i.next();
			Object l = e.getKey();
			
			if((k == null && l == null) ||
					(k != null && k.equals(l))) {
				char r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntryV<K> e : charValueEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@SuppressWarnings("unchecked")
	public boolean equals(Object obj) {
		if(obj instanceof CharacterValueMap) {
			CharacterValueMap<K> m = (CharacterValueMap<K>)obj;
			Iterator<PrimitiveEntryV<K>> i = charValueEntrySet().iterator();
			
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntryV<K> o = i.next();
				
				if(!m.containsKey(o.getKey()) ||
						m.getElement(o.getKey()) != o.getValue()) {
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
		for(PrimitiveEntryV<K> e : charValueEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
