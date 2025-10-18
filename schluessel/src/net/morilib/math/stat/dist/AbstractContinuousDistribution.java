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
package net.morilib.math.stat.dist;

import net.morilib.lang.transform.DoubleTransform;
import net.morilib.math.analysis.inexact.MonotonicFunctions;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/15
 */
public abstract class AbstractContinuousDistribution
implements ContinuousDistribution {

	/**
	 * 
	 */
	public final DoubleTransform cumulative = new DoubleTransform() {

		public double f(double x) {
			return cdf(x);
		}

	};

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double, double)
	 */
	public double cdf(double x1, double x2) {
		return cdf(x2) - cdf(x1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#invCdf(double)
	 */
	public double invCdf(double p) {
		if(p == 1.0) {
			return supportSupremum();
		} else if(p == 0.0) {
			return supportInfimum();
		} else if(0.0 < p && p < 1.0){
			return MonotonicFunctions.findRootByBisectionMethod(
					cumulative, p,
					supportInfimum(), supportSupremum());
		} else {
//			throw new IllegalArgumentException(p + "");
			return Double.NaN;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#median()
	 */
	public double median() {
		return invCdf(0.5);
	}

}
