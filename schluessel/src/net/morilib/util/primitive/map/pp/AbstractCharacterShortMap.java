/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, shortersion 2.0 (the "License");
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
import net.morilib.util.primitive.AbstractShortCollection;
import net.morilib.util.primitive.CharacterSet;
import net.morilib.util.primitive.ShortCollection;
import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.iterator.ShortIterator;
import net.morilib.util.primitive.map.op.ShortValueMap;
import net.morilib.util.primitive.map.po.CharacterMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractCharacterShortMap
implements CharacterShortMap {
	
	//
	private transient CharacterSet keySet = null;
	private transient ShortCollection values = null;
	private transient Set<PrimitiveEntryK<Short>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Character>>  entrySetV = null;
	private transient Set<Map.Entry<Character, Short>> entrySet  = null;
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#clear()
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
	 * @see net.morilib.util.primitive.map.op.ToShortMap#inRange(short)
	 */
	public boolean containsValueElement(short v) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getValue() == v) {
				return true;
			}
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#get(java.lang.Object)
	 */
	public short f(char k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.CharacterShortMap#getElement(char)
	 */
	public Short getElement(char k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#isEmpty()
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
			return AbstractCharacterShortMap.this.size();
		}

		public boolean containsChar(char o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterShortMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterShortMap.this.clear();
		}

		public boolean addChar(char v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#keySet()
	 */
	public CharacterSet charKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#putAll(net.morilib.util.primitive.map.op.ToShortMap)
	 */
	public void putAllElement(CharacterShortMap map) {
		for(PrimitiveEntry e : map.primitiveEntrySet()) {
			putElement(e.getKey(), e.getValue());
		}
	}
	
	//
	private class VCol extends AbstractShortCollection {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private VCol(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}

		public ShortIterator shortIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new ShortIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public short next() {
					return ei.next().getValue();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractCharacterShortMap.this.size();
		}

		public boolean addShort(short v) {
			throw new UnsupportedOperationException();
		}

		public boolean isInfinite() {
			return false;
		}

		public boolean containsShort(short o) {
			return containsValueElement(o);
		}

		public boolean remove(Object o) {
			return AbstractCharacterShortMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractCharacterShortMap.this.clear();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#values()
	 */
	public ShortCollection values() {
		return (values == null) ?
				(values = new VCol(primitiveEntrySet())) : values;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToShortMap#isTotal()
	 */
	public boolean isTotal() {
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsKey(java.lang.Object)
	 */
	public boolean containsKey(Object key) {
		if(key instanceof Short) {
			return containsKeyElement(((Character)key).charValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Short get(Object key) {
		if(key instanceof Short) {
			return getElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Short put(Character key, short value) {
		return putElement(key.charValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Short remove(Object key) {
		if(key instanceof Short) {
			return removeElement(((Character)key).charValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Character, ? extends Short> m) {
		for(Map.Entry<? extends Character, ? extends Short> e : m.entrySet()) {
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
	public Set<Entry<Character, Short>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Character, Short>>() {
	
				public Iterator<Map.Entry<Character, Short>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Character, Short>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Character, Short> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Character, Short>() {
	
								public Character getKey() {
									return o.getKey();
								}
	
								public Short getValue() {
									return o.getValue();
								}
	
								public Short setValue(Short value) {
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
	 * @see net.morilib.util.primitive.map.po.ShortMap#containsKey(int)
	 */
	public boolean containsKey(int k) {
		if(k < Character.MIN_VALUE || k > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#containsKeyShort(short)
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
	 * @see net.morilib.util.primitive.map.po.ShortMap#get(int)
	 */
	public Short get(int k) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return getElement((short)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#put(int, java.lang.Object)
	 */
	public Short put(int k, Short v) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return putElement((char)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#removeElement(short)
	 */
	public Short removeElement(char k) {
		Iterator<PrimitiveEntry> i;
		
		i = primitiveEntrySet().iterator();
		while(i.hasNext()) {
			PrimitiveEntry e = i.next();
			
			if(e.getKey() == k) {
				short r = e.getValue();
				
				i.remove();
				return r;
			}
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.ShortMap#remove(int)
	 */
	public Short remove(int k) {
		if(k < Short.MIN_VALUE || k > Short.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return removeElement((char)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#charEntrySet()
	 */
	public Set<PrimitiveEntryK<Short>> charKeyEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetK == null) {
			entrySetK = new AbstractSet<PrimitiveEntryK<Short>>() {
	
				public Iterator<PrimitiveEntryK<Short>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryK<Short>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryK<Short> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryK<Short>() {
	
								public char getKey() {
									return o.getKey();
								}
	
								public Short getValue() {
									return o.getValue();
								}
	
								public Short setValue(Short value) {
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
	public Short putElement(char k, Short v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#putAllElement(net.morilib.util.primitive.map.po.CharacterMap)
	 */
	public void putAllElement(CharacterMap<Short> map) {
		Iterator<CharacterMap.PrimitiveEntryK<Short>> i;
		
		i = map.charKeyEntrySet().iterator();
		while(i.hasNext()) {
			CharacterMap.PrimitiveEntryK<Short> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Character.MIN_VALUE || v > Character.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((short)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#shortValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Character>> shortValueEntrySet() {
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
	
								public short getValue() {
									return o.getValue();
								}
	
								public short setValue(short value) {
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
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#getElement(java.lang.Object)
	 */
	public short getElement(Object k) {
		if(k instanceof Character) {
			return f(((Character)k).charValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#putElement(java.lang.Object, short)
	 */
	public short putElement(Character k, short v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#putAllElement(net.morilib.util.primitive.map.op.ShortValueMap)
	 */
	public void putAllElement(ShortValueMap<Character> map) {
		Iterator<ShortValueMap.PrimitiveEntryV<Character>> i;
		
		i = map.shortValueEntrySet().iterator();
		while(i.hasNext()) {
			ShortValueMap.PrimitiveEntryV<Character> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ShortValueMap#shortValues()
	 */
	public ShortCollection shortValues() {
		return values();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#containsValue(java.lang.Object)
	 */
	public boolean containsValue(Object value) {
		if(value instanceof Short) {
			return containsValueElement(((Short)value).shortValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Short put(Character key, Short value) {
		return putElement(key.charValue(), value.shortValue());
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
		if(obj instanceof CharacterShortMap) {
			CharacterShortMap m = (CharacterShortMap)obj;
			Iterator<PrimitiveEntry> i;
			
			i = primitiveEntrySet().iterator();
			if(size() != m.size()) {
				return false;
			}
			while(i.hasNext()) {
				PrimitiveEntry o = i.next();
				short v = m.getElement(o.getKey());
				short p = o.getValue();
				
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
