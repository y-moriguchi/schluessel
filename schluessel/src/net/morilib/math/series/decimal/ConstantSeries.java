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
package net.morilib.math.series.decimal;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Iterator;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/04
 */
public class ConstantSeries implements Iterator<BigDecimal> {

	//
	private BigDecimal val;

	/**
	 * 
	 * @param v
	 */
	public ConstantSeries(BigInteger v) {
		val = new BigDecimal(v);
	}

	/**
	 * 
	 * @param v
	 */
	public ConstantSeries(BigDecimal v) {
		val = v;
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
	public BigDecimal next() {
		return val;
	}

	/* (non-Javadoc)
	 * @see java.util.Iterator#remove()
	 */
	public void remove() {
		throw new UnsupportedOperationException();
	}

}
