/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, charersion 2.0 (the "License");
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
package net.morilib.util.primitive.map.pp;

import java.util.AbstractSet;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import net.morilib.util.primitive.AbstractCharacterSet;
import net.morilib.util.primitive.AbstractCharacterCollection;
import net.morilib.util.primitive.CharacterSet;
import net.morilib.util.primitive.CharacterCollection;
import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.map.op.CharacterValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractCharacterCharacterMap
implements CharacterCharacterMap {
	
	//
	private transient CharacterSet keySet = null;
	private transient CharacterCollection values = null;
	private transient Set<PrimitiveEntryK<Character>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Character>>  entrySetV = null;
	private transient Set<Map.Entry<Character, Character>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#clear()
	 */
	public void clear() {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			i.next();
			i.remove();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#inRange(char)
	 */
	public boolean containsValueElement(char v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#get(java.lang.Object)
	 */
	public char f(char k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.CharacterCharacterMap#getElement(char)
	 */
	public Character getElement(char k) {
		return f(k);
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
		private Set<PrimitiveEntry> eset;
		
		private KSet(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}
		
		public CharacterIterator charIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractCharacterCharacterMap.this.size();
		}

		public boolean containsChar(char o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterCharacterMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterCharacterMap.this.clear();
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
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#putAll(net.morilib.util.primitive.map.op.ToCharacterMap)
	 */
	public void putAllElement(CharacterCharacterMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractCharacterCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public CharacterIterator charIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
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
			return AbstractCharacterCharacterMap.this.size();
		}

		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsChar(char o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterCharacterMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterCharacterMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#values()
	 */
	public CharacterCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
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
	public Character get(Object key) {
		if(key instanceof Character) {
			return getElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Character put(Character key, char value) {
		return putElement(key.charValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Character remove(Object key) {
		if(key instanceof Character) {
			return removeElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Character, ? extends Character> m) {
		for(Map.Entry<? extends Character, ? extends Character> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public CharacterSet keySet() {
		return charKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Character, Character>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Character, Character>>() {
	
				public Iterator<Map.Entry<Character, Character>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Character, Character>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Character, Character> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Character, Character>() {
	
								public Character getKey() {
									return o.getKey();
								}
	
								public Character getValue() {
									return o.getValue();
								}
	
								public Character setValue(Character value) {
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
		return entrySet;
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
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#get(int)
	 */
	public Character get(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#put(int, java.lang.Object)
	 */
	public Character put(int k, Character v) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((char)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#removeElement(char)
	 */
	public Character removeElement(char k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				char r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#remove(int)
	 */
	public Character remove(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#charEntrySet()
	 */
	public Set<PrimitiveEntryK<Character>> charKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Character>>() {
	
				public Iterator<PrimitiveEntryK<Character>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Character>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Character> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Character>() {
	
								public char getKey() {
									return o.getKey();
								}
	
								public Character getValue() {
									return o.getValue();
								}
	
								public Character setValue(Character value) {
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
		return entrySetK;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#putElement(char, java.lang.Object)
	 */
	public Character putElement(char k, Character v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#putAllElement(net.morilib.util.primitive.map.po.CharacterMap)
	 */
	public void putAllElement(CharacterMap<Character> map) {
		Iterator<CharacterMap.PrimitiveEntryK<Character>> i;
		
		i = map.charKeyEntrySet().iterator();
		while(i.hasNext()) {
			CharacterMap.PrimitiveEntryK<Character> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((char)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#charValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Character>> charValueEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetV == null) {
			entrySetV = new AbstractSet<PrimitiveEntryV<Character>>() {
	
				public Iterator<PrimitiveEntryV<Character>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryV<Character>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryV<Character> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryV<Character>() {
	
								public Character getKey() {
									return o.getKey();
								}
	
								public char getValue() {
									return o.getValue();
								}
	
								public char setValue(char value) {
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
		return entrySetV;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#getElement(java.lang.Object)
	 */
	public char getElement(Object k) {
		if(k instanceof Character) {
			return f(((Character)k).charValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#putElement(java.lang.Object, char)
	 */
	public char putElement(Character k, char v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#putAllElement(net.morilib.util.primitive.map.op.CharacterValueMap)
	 */
	public void putAllElement(CharacterValueMap<Character> map) {
		Iterator<CharacterValueMap.PrimitiveEntryV<Character>> i;
		
		i = map.charValueEntrySet().iterator();
		while(i.hasNext()) {
			CharacterValueMap.PrimitiveEntryV<Character> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#charValues()
	 */
	public CharacterCollection charValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Character) {
			return containsValueElement(((Character)value).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Character put(Character key, Character value) {
		return putElement(key.charValue(), value.charValue());
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(PrimitiveEntry e : primitiveEntrySet()) {
			r += e.hashCode();
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof CharacterCharacterMap) {
			CharacterCharacterMap m = (CharacterCharacterMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				char v = m.getElement(o.getKey());
				char p = o.getValue();
				
				if(!m.containsKeyElement(o.getKey()) || v != p) {
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
		for(PrimitiveEntry e : primitiveEntrySet()) {
			b.append(d);
			b.append(e);
			d = ", ";
		}
		b.append("}");
		return b.toString();
	}
	
}
