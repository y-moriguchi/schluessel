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
public class ContinuousUniformDistribution
extends AbstractContinuousDistribution {

	//
	private double a, b;

	/**
	 * @param a
	 * @param b
	 */
	public ContinuousUniformDistribution(double a, double b) {
		if(a >= b)  throw new IllegalArgumentException();
		this.a = a;
		this.b = b;
	}

	/**
	 * @return the a
	 */
	public double getA() {
		return a;
	}

	/**
	 * @return the b
	 */
	public double getB() {
		return b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#expectedValue()
	 */
	public double expectedValue() {
		return (a + b) / 2.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		return (b - a) * (b - a) / 12.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return (a + b) / 2.0;  // any x in [a, b]
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
		return -1.2;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#f(double)
	 */
	public double f(double x) {
		return (a <= x && x <= b) ? 1 / (b - a) : 0.0;
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
		if(x < a) {
			return 0;
		} else if(x < b) {
			return (x - a) / (b - a);
		} else {
			return 1;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#supportSupremum()
	 */
	public double supportSupremum() {
		return b;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#supportInfimum()
	 */
	public double supportInfimum() {
		return a;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractContinuousDistribution#invCdf(double)
	 */
	@Override
	public double invCdf(double p) {
		if(p < a) {
			return 0;
		} else if(p < b) {
			return p * (b - a) + a;
		} else {
			return 1;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractContinuousDistribution#median()
	 */
	@Override
	public double median() {
		return (a + b) / 2.0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (r + (int)Double.doubleToLongBits(a));
		r = Hashes.A * (r + (int)Double.doubleToLongBits(b));
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	public boolean equals(Object o) {
		ContinuousUniformDistribution n;

		if(o instanceof ContinuousUniformDistribution) {
			n = (ContinuousUniformDistribution)o;
			return a == n.a && b == n.b;
		}
		return false;
	}

}
