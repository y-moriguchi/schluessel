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
package net.morilib.lisp.charset;

import java.util.Iterator;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.lisp.Datum2;
import net.morilib.range.integer.IntInterval;
import net.morilib.range.integer.IntRangeAdder;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class LispCharSetBuilder extends Datum2 {

	//
	/*package*/ static class Int2 implements Comparable<Int2> {

		//
		/*package*/ int left, right;

		//
		/*package*/ Int2() { }

		//
		/*package*/ Int2(int left, int right) {
			this.left = left;
			this.right = right;
		}

		/* (non-Javadoc)
		 * @see java.lang.Comparable#compareTo(java.lang.Object)
		 */
		public int compareTo(Int2 o) {
			if(left < o.left) {
				return -1;
			} else if(left > o.left) {
				return 1;
			} else if(right < o.right) {
				return -1;
			} else if(right > o.right) {
				return 1;
			} else {
				return 0;
			}
		}

	}

	//
	/*package*/ LispCharSet range;
	/*package*/ SortedSet<Int2> set;

	//
	/*package*/ LispCharSetBuilder(LispCharSet base) {
		this.range = base;
		this.set   = new TreeSet<Int2>();
	}

	/**
	 * 
	 * @param c
	 */
	public void add(int c) {
		Iterator<LispCharSetBuilder.Int2> itr;
		LispCharSetBuilder.Int2 x, y;

		itr = set.iterator();
		while(itr.hasNext()) {
			x = itr.next();
			if(x.left <= c && c <= x.right) {
				return;
			} else if(x.right + 1 == c) {
				if(itr.hasNext()) {
					y = itr.next();
					if(x.right + 1 == y.left) {
						x.right = y.right;
						itr.remove();
					} else {
						x.right++;
					}
				} else {
					x.right++;
				}
				return;
			}
		}
		set.add(new Int2(c, c));
	}

	/**
	 * 
	 * @return
	 */
	public LispCharSet toCharSet() {
		IntRangeAdder r = new IntRangeAdder();

		for(LispCharSetBuilder.Int2 x : set) {
			r.add(new IntInterval(x.left, x.right));
		}

		if(range != null) {
			range.charset = range.charset.join(r.toRange());
			return range;
		} else {
			return new LispCharSet(r.toRange());
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<char-set-builder>");
	}

}
