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

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;

import net.morilib.lisp.Datum2;
import net.morilib.range.integer.IntInterval;
import net.morilib.range.integer.IntRangeAdder;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/10
 */
public class LispCharSetRemover extends Datum2 {

	//
	/*package*/ LispCharSet range;
	/*package*/ List<LispCharSetBuilder.Int2> set;

	//
	/*package*/ LispCharSetRemover(LispCharSet base) {
		if(base == null) {
			throw new NullPointerException();
		}
		this.range = base;
		this.set   = new ArrayList<LispCharSetBuilder.Int2>();
		for(IntInterval i : this.range.charset.intervals()) {
			set.add(new LispCharSetBuilder.Int2(
					i.minimum(), i.maximum()));
		}
	}

	/**
	 * 
	 * @param c
	 */
	public void remove(int c) {
		ListIterator<LispCharSetBuilder.Int2> itr;
		LispCharSetBuilder.Int2 x, y;

		itr = set.listIterator();
		while(itr.hasNext()) {
			x = itr.next();
			if(c == x.left) {
				if(c == x.right) {
					itr.remove();
				} else {
					x.left++;
				}
				break;
			} else if(c == x.right) {
				x.right--;
				break;
			} else if(x.left < c && c < x.right) {
				y = new LispCharSetBuilder.Int2(c + 1, x.right);
				x.right = c - 1;
				itr.add(y);
				break;
			}
		}
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
		range.charset = r.toRange();
		return range;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<char-set-remover>");
	}

}
