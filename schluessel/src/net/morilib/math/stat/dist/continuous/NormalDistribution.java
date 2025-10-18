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
import net.morilib.math.special.Erf;
import net.morilib.math.stat.DistributionUtils;
import net.morilib.math.stat.dist.AbstractContinuousDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public class NormalDistribution
extends AbstractContinuousDistribution {

	//
	private double mean, variance;

	/**
	 * @param mean2
	 * @param variance2
	 */
	public NormalDistribution(double mean, double variance) {
		if(variance <= 0)  throw new IllegalArgumentException();
		this.mean = mean;
		this.variance = variance;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#expectedValue()
	 */
	public double expectedValue() {
		return mean;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		return variance;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return mean;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#skewness()
	 */
	public double skewness() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#kurtosis()
	 */
	public double kurtosis() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#f(double)
	 */
	public double f(double x) {
		double k, a;

		k = -((x - mean) * (x - mean) / 2.0 / variance);
		a = 1.0 / Math.sqrt(2 * Math.PI * variance);
		return a * Math.exp(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#isInSupport(double)
	 */
	public boolean isInSupport(double n) {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double)
	 */
	public double cdf(double x) {
		return DistributionUtils.trimCdf(
				0.5 * (1 +
						Erf.erf((x - mean) / Math.sqrt(2 * variance))),
				1e-13);
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
		return Double.NEGATIVE_INFINITY;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + (int)Double.doubleToLongBits(mean));
		r = Hashes.A * (r + (int)Double.doubleToLongBits(variance));
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		NormalDistribution n;

		if(o instanceof NormalDistribution) {
			n = (NormalDistribution)o;
			return mean == n.mean && variance == n.variance;
		}
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractContinuousDistribution#median()
	 */
	@Override
	public double median() {
		return mean;
	}

}
