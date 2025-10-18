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
package net.morilib.util.primitive;

import net.morilib.util.primitive.iterator.CharacterIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractCharacterSet extends AbstractCharacterCollection
implements CharacterSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterCollection#toSet()
	 */
	public final CharacterSet toSet() {
		return CharacterCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.CharacterSet#collect(net.morilib.util.primitive.CharacterSet)
	 */
	public CharacterSet collect(CharacterSet set) {
		CharacterSet r = new CharacterHashSet(this);  // %BITSET% char
		
		r.removeAllChar(set);
		return CharacterCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(CharacterIterator i = charIterator(); i.hasNext();) {
			// %HASHCODE% char
			r += (int)(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof CharacterCollection) {
			CharacterCollection m = (CharacterCollection)obj;
			
			return containsAllChar(m) && m.containsAllChar(this);
		}
		return false;
	}

}
