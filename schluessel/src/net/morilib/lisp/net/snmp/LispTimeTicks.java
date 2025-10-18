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
package net.morilib.lisp.net.snmp;

import net.morilib.lang.algebra.Addable;
import net.morilib.lang.algebra.Subtractable;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispArithmeticException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/15
 */
public class LispTimeTicks extends Datum2
implements Addable<LispTimeTicks>, Subtractable<LispTimeTicks> {

	//
	private long ticks;

	/**
	 * 
	 * @param x
	 */
	public LispTimeTicks(long x) {
		if((ticks = x) < 0) {
			throw new IllegalArgumentException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Subtractable#subtract(java.lang.Object)
	 */
	public LispTimeTicks subtract(LispTimeTicks x) {
		return new LispTimeTicks(ticks + x.ticks);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#add(java.lang.Object)
	 */
	public LispTimeTicks add(LispTimeTicks x) {
		long t = ticks - x.ticks;

		if(t < 0) {
			throw new LispArithmeticException("not negative");
		}
		return new LispTimeTicks(t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispTimeTicks multiply(int n) {
		return new LispTimeTicks(ticks * n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append(String.format("#<time-ticks %d.%03dsec>",
				ticks / 1000, ticks % 1000));
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		return (int)ticks;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		if(o instanceof LispTimeTicks) {
			return ticks == ((LispTimeTicks)o).ticks;
		}
		return false;
	}

}
