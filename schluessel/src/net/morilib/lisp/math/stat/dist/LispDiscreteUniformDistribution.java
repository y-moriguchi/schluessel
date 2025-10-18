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
package net.morilib.lisp.math.stat.dist;

import net.morilib.math.stat.dist.DiscreteDistribution;
import net.morilib.math.stat.dist.discrete.DiscreteUniformDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class LispDiscreteUniformDistribution
extends AbstractLispDistribution {

	//
	private DiscreteUniformDistribution dist;

	/**
	 * 
	 * @param a
	 * @param b
	 */
	public LispDiscreteUniformDistribution(int a, int b) {
		dist = new DiscreteUniformDistribution(a, b);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.LispContinuousDistribution#getDistribution()
	 */
	@Override
	public DiscreteDistribution getDistribution() {
		return dist;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<discrete U(").append(dist.getA()).append(". ");
		buf.append(dist.getB()).append(")>");
	}

}
