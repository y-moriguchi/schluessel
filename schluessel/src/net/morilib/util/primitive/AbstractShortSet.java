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

import net.morilib.util.primitive.iterator.ShortIterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractShortSet extends AbstractShortCollection
implements ShortSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortCollection#toSet()
	 */
	public final ShortSet toSet() {
		return ShortCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ShortSet#collect(net.morilib.util.primitive.ShortSet)
	 */
	public ShortSet collect(ShortSet set) {
		ShortSet r = new ShortHashSet(this);  // %BITSET% short
		
		r.removeAllShort(set);
		return ShortCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(ShortIterator i = shortIterator(); i.hasNext();) {
			// %HASHCODE% short
			r += (int)(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof ShortCollection) {
			ShortCollection m = (ShortCollection)obj;
			
			return containsAllShort(m) && m.containsAllShort(this);
		}
		return false;
	}

}
