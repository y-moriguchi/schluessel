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
package net.morilib.math.stat.dist.continuous;

import net.morilib.lang.Hashes;
import net.morilib.math.stat.dist.AbstractContinuousDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public class ExponentialDistribution
extends AbstractContinuousDistribution {

	//
	private static final double LN2 = Math.log(2);

	//
	private double lambda;

	/**
	 * @param lambda2
	 */
	public ExponentialDistribution(double lambda) {
		this.lambda = lambda;
	}

	/**
	 * @return the lambda
	 */
	public double getLambda() {
		return lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#expectedValue()
	 */
	public double expectedValue() {
		return 1.0 / lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		return 1.0 / lambda / lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#skewness()
	 */
	public double skewness() {
		return 2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#kurtosis()
	 */
	public double kurtosis() {
		return 6;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#f(double)
	 */
	public double f(double x) {
		return (x < 0) ? Double.NaN : lambda * Math.exp(-lambda * x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#isInSupport(double)
	 */
	public boolean isInSupport(double n) {
		return n >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double)
	 */
	public double cdf(double x) {
		return (x < 0) ? Double.NaN : 1.0 - Math.exp(-lambda * x);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#supportSupremum()
	 */
	public double supportSupremum() {
		return Double.POSITIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#supportInfimum()
	 */
	public double supportInfimum() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractContinuousDistribution#median()
	 */
	@Override
	public double median() {
		return LN2 / lambda;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + (int)Double.doubleToLongBits(lambda));
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		ExponentialDistribution n;

		if(o instanceof ExponentialDistribution) {
			n = (ExponentialDistribution)o;
			return lambda == n.lambda;
		}
		return false;
	}

}
