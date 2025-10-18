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
package net.morilib.math.stat.dist.discrete;

import net.morilib.lang.Hashes;
import net.morilib.math.stat.dist.AbstractDiscreteDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public class DiscreteUniformDistribution
extends AbstractDiscreteDistribution {

	//
	private int a, b;

	/**
	 * @param a2
	 * @param b2
	 */
	public DiscreteUniformDistribution(int a, int b) {
		if(a < 0 || b < 0) {
			throw new IllegalArgumentException();
		} else if(a >= b) {
			throw new IllegalArgumentException();
		}
		this.a = a;
		this.b = b;
	}

	/**
	 * @return the a
	 */
	public int getA() {
		return a;
	}

	/**
	 * @return the b
	 */
	public int getB() {
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
		double n = (double)(b - a + 1);

		return (n * n - 1) / 12.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return (a + b) / 2.0;  // any x in [a, b]
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#median()
	 */
	public double median() {
		return (a + b) / 2.0;
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
		double n = (double)(b - a + 1);

		return -1.2 * (n * n + 1) / (n * n - 1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#f(double)
	 */
	public double f(int x) {
		return (a <= x && x <= b) ? 1.0 / (b - a + 1) : 0.0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#isInSupport(double)
	 */
	public boolean isInSupport(int n) {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#cdf(int)
	 */
	public double cdf(double x) {
		double n = (double)(b - a + 1);

		if(x < a) {
			return 0;
		} else if(x < b) {
			return (Math.floor(x) - a + 1) / n;
		} else {
			return 1;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#supportMinimum()
	 */
	public int supportMinimum() {
		return 0;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#hashCode()
	 */
	@Override
	public int hashCode() {
		int r = Hashes.INIT;

		r = Hashes.A * (a + r);
		r = Hashes.A * (b + r);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof BinomialDistribution) {
			DiscreteUniformDistribution d;

			d = (DiscreteUniformDistribution)o;
			return a == d.a && b == d.b;
		}
		return false;
	}

}
