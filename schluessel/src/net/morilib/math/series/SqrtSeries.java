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
package net.morilib.math.series;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;

import net.morilib.lang.number.Rational;
import net.morilib.math.Math3;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class SqrtSeries implements Iterator<Rational> {

	//
	private Iterator<BigDecimal> iter;
	private BigDecimal bef;
	private boolean stat = false;
	private int sc = 0;

	/**
	 * 
	 * @param iter
	 */
	public SqrtSeries(Iterator<BigDecimal> iter) {
		this.iter = iter;
		bef = iter.next();
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#hasNext()
	 */
	public boolean hasNext() {
		return true;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#next()
	 */
	public Rational next() {
		BigDecimal d1, dd, r;

		d1 = iter.next();
		if((dd = bef.subtract(d1)).signum() != 0) {
			for(sc = 0; dd.intValue() == 0; sc++) {
				dd = dd.movePointRight(1);
			}
		} else {
			sc += 3;
		}

		r = (sc > 0) ? Math3.sqrtDecimal(d1, --sc) : d1;
		if(stat) {
			r = r.add(new BigDecimal(BigInteger.ONE, sc));
		}
		stat = !stat;
		bef  = d1;
		return Rational.valueOf(r);
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
