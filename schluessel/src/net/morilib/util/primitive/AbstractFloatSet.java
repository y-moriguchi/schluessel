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

import net.morilib.util.primitive.iterator.FloatIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractFloatSet extends AbstractFloatCollection
implements FloatSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatCollection#toSet()
	 */
	public final FloatSet toSet() {
		return FloatCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.FloatSet#collect(net.morilib.util.primitive.FloatSet)
	 */
	public FloatSet collect(FloatSet set) {
		FloatSet r = new FloatHashSet(this);  // %BITSET% float
		
		r.removeAllFloat(set);
		return FloatCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(FloatIterator i = floatIterator(); i.hasNext();) {
			// %HASHCODE% float
			r += Float.floatToIntBits(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof FloatCollection) {
			FloatCollection m = (FloatCollection)obj;
			
			return containsAllFloat(m) && m.containsAllFloat(this);
		}
		return false;
	}

}
