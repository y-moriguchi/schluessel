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
import net.morilib.lang.transform.DoubleTransform;
import net.morilib.math.special.Gamma2;
import net.morilib.math.stat.dist.AbstractContinuousDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class GammaDistribution extends AbstractContinuousDistribution {

	//
	double k, theta;
	private final DoubleTransform lningamma;
	private final double lngammak;

	/**
	 * 
	 * @param k
	 * @param theta
	 */
	public GammaDistribution(double k, double theta) {
		if(k <= 0 || theta <= 0) {
			throw new IllegalArgumentException();
		}
		this.k     = k;
		this.theta = theta;
		lngammak   = Gamma2.lnGamma(k);
		lningamma  = Gamma2.lnIncompleteGammaLowerFunction(k);
	}

	/**
	 * 
	 * @param k
	 * @return
	 */
	public static GammaDistribution chiSquared(int k) {
		if(k <= 0)  throw new IllegalArgumentException();
		return new GammaDistribution(k / 2.0, 2.0);
	}

	/**
	 * 
	 * @param k
	 * @return
	 */
	public static GammaDistribution erlang(int k, double lambda) {
		if(k <= 0)  throw new IllegalArgumentException();
		return new GammaDistribution(k, 1.0 / lambda);
	}

	/**
	 * @return the k
	 */
	public double getK() {
		return k;
	}

	/**
	 * @return the theta
	 */
	public double getTheta() {
		return theta;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		double r;

		if(x < 0) {
			return 0.0;
		} else if(x == 0.0) {
			return (k == 1) ? 1 / theta : 0.0;
		} else {
			r  = (k - 1) * Math.log(x) - x / theta;
			r -= k * Math.log(theta) + lngammak;
			return Math.exp(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double)
	 */
	public double cdf(double x) {
		return (x > 0) ?
				Math.exp(lningamma.f(x / theta) - lngammak) : 0.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#isInSupport(double)
	 */
	public boolean isInSupport(double n) {
		return n >= 0;
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
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#expectedValue()
	 */
	public double expectedValue() {
		return k * theta;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		return k * theta * theta;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return (k < 1) ? 0 : (k - 1) * theta;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#skewness()
	 */
	public double skewness() {
		return 2 / Math.sqrt(k);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#kurtosis()
	 */
	public double kurtosis() {
		return 6.0 / k;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + (int)Double.doubleToLongBits(k));
		r = Hashes.A * (r + (int)Double.doubleToLongBits(theta));
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		GammaDistribution n;

		if(o instanceof GammaDistribution) {
			n = (GammaDistribution)o;
			return k == n.k && theta == n.theta;
		}
		return false;
	}

}
