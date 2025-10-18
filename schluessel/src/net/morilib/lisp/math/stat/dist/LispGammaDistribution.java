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

import net.morilib.math.stat.dist.ContinuousDistribution;
import net.morilib.math.stat.dist.continuous.GammaDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class LispGammaDistribution extends AbstractLispDistribution {

	//
	GammaDistribution dist;

	/**
	 * 
	 * @param dist
	 */
	public LispGammaDistribution(GammaDistribution dist) {
		this.dist = dist;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.LispContinuousDistribution#getDistribution()
	 */
	@Override
	public ContinuousDistribution getDistribution() {
		return dist;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		if(Math.IEEEremainder(dist.getK(), 0.5) == 0.0 &&
				dist.getTheta() == 2.0) {  // chi squared
			buf.append("#<chi^2(k=").append((int)(dist.getK() * 2));
		} else if(Math.IEEEremainder(dist.getK(), 1.0) == 0.0) {
			buf.append("#<Erlang(k=").append((int)dist.getK());
			buf.append(", lambda=").append(1.0 / dist.getTheta());
		} else {
			buf.append("#<gamma(k=").append(dist.getK());
			buf.append(", theta=").append(dist.getTheta());
		}
		buf.append(")>");
	}

}
