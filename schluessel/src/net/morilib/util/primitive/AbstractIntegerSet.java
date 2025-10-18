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

import net.morilib.util.primitive.iterator.IntegerIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractIntegerSet extends AbstractIntegerCollection
implements IntegerSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerCollection#toSet()
	 */
	public final IntegerSet toSet() {
		return IntegerCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.IntegerSet#collect(net.morilib.util.primitive.IntegerSet)
	 */
	public IntegerSet collect(IntegerSet set) {
		IntegerSet r = new IntegerHashSet(this);  // %BITSET% int
		
		r.removeAllInt(set);
		return IntegerCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(IntegerIterator i = intIterator(); i.hasNext();) {
			// %HASHCODE% int
			r += (int)(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof IntegerCollection) {
			IntegerCollection m = (IntegerCollection)obj;
			
			return containsAllInt(m) && m.containsAllInt(this);
		}
		return false;
	}

}
