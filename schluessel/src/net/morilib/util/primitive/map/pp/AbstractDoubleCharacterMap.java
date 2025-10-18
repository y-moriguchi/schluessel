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

import net.morilib.util.primitive.AbstractDoubleSet;
import net.morilib.util.primitive.AbstractCharacterCollection;
import net.morilib.util.primitive.DoubleSet;
import net.morilib.util.primitive.CharacterCollection;
import net.morilib.util.primitive.iterator.DoubleIterator;
import net.morilib.util.primitive.iterator.CharacterIterator;
import net.morilib.util.primitive.map.op.CharacterValueMap;
import net.morilib.util.primitive.map.po.DoubleMap;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/30
 */
public abstract class AbstractDoubleCharacterMap
implements DoubleCharacterMap {
	
	//
	private transient DoubleSet keySet = null;
	private transient CharacterCollection values = null;
	private transient Set<PrimitiveEntryK<Character>> entrySetK = null;
	private transient Set<PrimitiveEntryV<Double>>  entrySetV = null;
	private transient Set<Map.Entry<Double, Character>> entrySet  = null;
	
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
	public char f(double k) {
		for(PrimitiveEntry e : primitiveEntrySet()) {
			if(e.getKey() == k) {
				return e.getValue();
			}
		}
		throw new NoSuchElementException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.pp.DoubleCharacterMap#getElement(double)
	 */
	public Character getElement(double k) {
		return f(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#isEmpty()
	 */
	public boolean isEmpty() {
		return size() == 0;
	}
	
	//
	private class KSet extends AbstractDoubleSet {
		
		//
		private Set<PrimitiveEntry> eset;
		
		private KSet(Set<PrimitiveEntry> eset) {
			this.eset = eset;
		}
		
		public DoubleIterator doubleIterator() {
			final Iterator<PrimitiveEntry> ei = eset.iterator();
			
			return new DoubleIterator() {

				public boolean hasNext() {
					return ei.hasNext();
				}

				public double next() {
					return ei.next().getKey();
				}

				public void remove() {
					ei.remove();
				}
				
			};
		}

		public int size() {
			return AbstractDoubleCharacterMap.this.size();
		}

		public boolean containsDouble(double o) {
			return containsKeyElement(o);
		}

		public boolean remove(Object o) {
			return AbstractDoubleCharacterMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleCharacterMap.this.clear();
		}

		public boolean addDouble(double v) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#keySet()
	 */
	public DoubleSet doubleKeySet() {
		return (keySet == null) ?
				(keySet = new KSet(primitiveEntrySet())) : keySet;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.ToCharacterMap#putAll(net.morilib.util.primitive.map.op.ToCharacterMap)
	 */
	public void putAllElement(DoubleCharacterMap map) {
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
			return AbstractDoubleCharacterMap.this.size();
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
			return AbstractDoubleCharacterMap.this.remove(o) != null;
		}

		public void clear() {
			AbstractDoubleCharacterMap.this.clear();
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
			return containsKeyElement(((Double)key).doubleValue());
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#get(java.lang.Object)
	 */
	public Character get(Object key) {
		if(key instanceof Character) {
			return getElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#put(java.lang.Object, java.lang.Object)
	 */
	public Character put(Double key, char value) {
		return putElement(key.doubleValue(), value);
	}

	/* (non-Javadoc)
	 * @see java.util.Map#remove(java.lang.Object)
	 */
	public Character remove(Object key) {
		if(key instanceof Character) {
			return removeElement(((Double)key).doubleValue());
		}
		return null;
	}

	/* (non-Javadoc)
	 * @see java.util.Map#putAll(java.util.Map)
	 */
	public void putAll(Map<? extends Double, ? extends Character> m) {
		for(Map.Entry<? extends Double, ? extends Character> e : m.entrySet()) {
			put(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see java.util.Map#keySet()
	 */
	public DoubleSet keySet() {
		return doubleKeySet();
	}

	/* (non-Javadoc)
	 * @see java.util.Map#entrySet()
	 */
	public Set<Entry<Double, Character>> entrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySet == null) {
			entrySet = new AbstractSet<Entry<Double, Character>>() {
	
				public Iterator<Map.Entry<Double, Character>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<Entry<Double, Character>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public Map.Entry<Double, Character> next() {
							final PrimitiveEntry o = i.next();
							
							return new Map.Entry<Double, Character>() {
	
								public Double getKey() {
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
		if(k < Double.MIN_VALUE || k > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsKeyElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#containsKeyChar(char)
	 */
	public boolean containsKeyElement(double k) {
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
		return putElement((double)k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.CharacterMap#removeElement(char)
	 */
	public Character removeElement(double k) {
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
		return removeElement((double)k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#doubleEntrySet()
	 */
	public Set<PrimitiveEntryK<Character>> doubleKeyEntrySet() {
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
	
								public double getKey() {
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
	 * @see net.morilib.util.primitive.map.po.DoubleMap#putElement(double, java.lang.Object)
	 */
	public Character putElement(double k, Character v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.po.DoubleMap#putAllElement(net.morilib.util.primitive.map.po.DoubleMap)
	 */
	public void putAllElement(DoubleMap<Character> map) {
		Iterator<DoubleMap.PrimitiveEntryK<Character>> i;
		
		i = map.doubleKeyEntrySet().iterator();
		while(i.hasNext()) {
			DoubleMap.PrimitiveEntryK<Character> e = i.next();
			
			putElement(e.getKey(), e.getValue());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#containsValue(int)
	 */
	public boolean containsValue(int v) {
		if(v < Double.MIN_VALUE || v > Double.MAX_VALUE) {
			throw new IllegalArgumentException();
		}
		return containsValueElement((char)v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#charValueEntrySet()
	 */
	public Set<PrimitiveEntryV<Double>> charValueEntrySet() {
		final Set<PrimitiveEntry> e = primitiveEntrySet();
		
		if(entrySetV == null) {
			entrySetV = new AbstractSet<PrimitiveEntryV<Double>>() {
	
				public Iterator<PrimitiveEntryV<Double>> iterator() {
					final Iterator<PrimitiveEntry> i = e.iterator();
					
					return new Iterator<PrimitiveEntryV<Double>>() {
	
						public boolean hasNext() {
							return i.hasNext();
						}
	
						public PrimitiveEntryV<Double> next() {
							final PrimitiveEntry o = i.next();
							
							return new PrimitiveEntryV<Double>() {
	
								public Double getKey() {
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
		if(k instanceof Double) {
			return f(((Double)k).doubleValue());
		}
		throw new IllegalArgumentException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#putElement(java.lang.Object, char)
	 */
	public char putElement(Double k, char v) {
		return putElement(k, v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.map.op.CharacterValueMap#putAllElement(net.morilib.util.primitive.map.op.CharacterValueMap)
	 */
	public void putAllElement(CharacterValueMap<Double> map) {
		Iterator<CharacterValueMap.PrimitiveEntryV<Double>> i;
		
		i = map.charValueEntrySet().iterator();
		while(i.hasNext()) {
			CharacterValueMap.PrimitiveEntryV<Double> e = i.next();
			
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
	public Character put(Double key, Character value) {
		return putElement(key.doubleValue(), value.charValue());
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
		if(obj instanceof DoubleCharacterMap) {
			DoubleCharacterMap m = (DoubleCharacterMap)obj;
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
