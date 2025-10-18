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
package net.morilib.lisp.math.stat.data;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.stat.dist.ILispDiscreteDistribution;
import net.morilib.math.stat.dist.DiscreteDistribution;
import net.morilib.math.stat.dist.Histogram;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class LispHistogram extends Datum2
implements ILispDiscreteDistribution {

	//
	Histogram histogram;

	/**
	 * 
	 * @param vals
	 */
	public LispHistogram(double... vals) {
		histogram = new Histogram(vals);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispIntegrable1#integrate(net.morilib.lisp.LispReal, net.morilib.lisp.LispReal)
	 */
	public LispReal integrate(LispReal a, LispReal b) {
		return new LispDouble(getDistribution().cdf(
				a.doubleValue(), b.doubleValue()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.ILispDistribution#getDistribution()
	 */
	public DiscreteDistribution getDistribution() {
		return histogram;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<histogram>");
	}

}
