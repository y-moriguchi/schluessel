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

import net.morilib.util.primitive.iterator.ByteIterator;

/**
 *
 * @author MORIGUCHI, Yuichiro 2010/10/11
 */
public abstract class AbstractByteSet extends AbstractByteCollection
implements ByteSet {

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#isInfinite()
	 */
	public final boolean isInfinite() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteCollection#toSet()
	 */
	public final ByteSet toSet() {
		return ByteCollections.unmodifiableSet(this);
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.util.primitive.ByteSet#collect(net.morilib.util.primitive.ByteSet)
	 */
	public ByteSet collect(ByteSet set) {
		ByteSet r = new ByteBitSet(this);  // %BITSET% byte
		
		r.removeAllByte(set);
		return ByteCollections.unmodifiableSet(r);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = 0;
		
		for(ByteIterator i = byteIterator(); i.hasNext();) {
			// %HASHCODE% byte
			r += (int)(i.next());
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object obj) {
		if(obj instanceof ByteCollection) {
			ByteCollection m = (ByteCollection)obj;
			
			return containsAllByte(m) && m.containsAllByte(this);
		}
		return false;
	}

}
