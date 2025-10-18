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

import java.math.BigInteger;

import net.morilib.math.stat.dist.Distribution;
import net.morilib.math.stat.dist.discrete.BinomialDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/21
 */
public class LispBinomialDistribution
extends AbstractLispDistribution {

	//
	BinomialDistribution dist;

	/**
	 * @param d
	 */
	public LispBinomialDistribution(BigInteger trial, double succeed) {
		dist = new BinomialDistribution(trial, succeed);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.AbstractLispDistribution#getDistribution()
	 */
	@Override
	public Distribution getDistribution() {
		return dist;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.AbstractLispDistribution#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<B(").append(dist.getTrial()).append(", ");
		buf.append(dist.getSucceed()).append(")>");
	}

}
