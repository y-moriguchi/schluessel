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
import net.morilib.math.special.BetaFunction;
import net.morilib.math.special.Gamma2;
import net.morilib.math.stat.dist.AbstractContinuousDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class TDistribution extends AbstractContinuousDistribution {

	//
	double nu;
	private final double lngammaNu1, lngammaNu, lnSqrtNuPi;

	/**
	 * 
	 * @param nu
	 */
	public TDistribution(double nu) {
		if(nu <= 0)  throw new IllegalArgumentException();
		this.nu = nu;
		lngammaNu1 = Gamma2.lnGamma((nu + 1) / 2);
		lngammaNu  = Gamma2.lnGamma(nu / 2);
		lnSqrtNuPi = Math.log(nu * Math.PI) / 2;
	}

	/**
	 * @return the nu
	 */
	public double getNu() {
		return nu;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		return Math.exp(lngammaNu1 - lnSqrtNuPi - lngammaNu -
				((nu + 1) / 2) * Math.log(1 + x * x / nu));
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double)
	 */
	public double cdf(double x) {
		double tau = nu / (x * x + nu);
		double r0  = 1.0 - BetaFunction.I(tau, nu / 2, 0.5) / 2;

		return (x < 0) ? 1 - r0 : r0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#isInSupport(double)
	 */
	public boolean isInSupport(double n) {
		return true;
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
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#expectedValue()
	 */
	public double expectedValue() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		if(nu <= 1) {
			return Double.NaN;
		} else if(nu <= 2) {
			return Double.POSITIVE_INFINITY;
		} else {
			return (nu - 2) / nu;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractContinuousDistribution#median()
	 */
	@Override
	public double median() {
		return 0;
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
		return (nu > 4) ? 6.0 / (nu - 4) : Double.NaN;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + (int)Double.doubleToLongBits(nu));
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		TDistribution n;

		if(o instanceof TDistribution) {
			n = (TDistribution)o;
			return nu == n.nu;
		}
		return false;
	}

}
