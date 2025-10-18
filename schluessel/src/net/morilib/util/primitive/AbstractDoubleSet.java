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

import net.morilib.util.primitive.iterator.DoubleIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractDoubleSet extends AbstractDoubleCollection
implements DoubleSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleCollection#toSet()
	 */
	public final DoubleSet toSet() {
		return DoubleCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.DoubleSet#collect(net.morilib.util.primitive.DoubleSet)
	 */
	public DoubleSet collect(DoubleSet set) {
		DoubleSet r = new DoubleHashSet(this);  // %BITSET% double
		
		r.removeAllDouble(set);
		return DoubleCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(DoubleIterator i = doubleIterator(); i.hasNext();) {
			// %HASHCODE% double
			r += (int)Double.doubleToLongBits(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof DoubleCollection) {
			DoubleCollection m = (DoubleCollection)obj;
			
			return containsAllDouble(m) && m.containsAllDouble(this);
		}
		return false;
	}

}
