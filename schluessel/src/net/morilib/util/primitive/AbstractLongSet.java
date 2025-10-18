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

import net.morilib.util.primitive.iterator.LongIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractLongSet extends AbstractLongCollection
implements LongSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongCollection#toSet()
	 */
	public final LongSet toSet() {
		return LongCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.LongSet#collect(net.morilib.util.primitive.LongSet)
	 */
	public LongSet collect(LongSet set) {
		LongSet r = new LongHashSet(this);  // %BITSET% long
		
		r.removeAllLong(set);
		return LongCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(LongIterator i = longIterator(); i.hasNext();) {
			// %HASHCODE% long
			r += (int)(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof LongCollection) {
			LongCollection m = (LongCollection)obj;
			
			return containsAllLong(m) && m.containsAllLong(this);
		}
		return false;
	}

}
