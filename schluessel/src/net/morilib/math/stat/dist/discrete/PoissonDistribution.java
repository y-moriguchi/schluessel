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

import net.morilib.math.special.Gamma2;
import net.morilib.math.stat.dist.AbstractDiscreteDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public class PoissonDistribution extends AbstractDiscreteDistribution {

	//
	private double lambda;

	/**
	 * @param lambda2
	 */
	public PoissonDistribution(double lambda) {
		if(lambda <= 0)  throw new IllegalArgumentException();
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
		return lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#variance()
	 */
	public double variance() {
		return lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#mode()
	 */
	public double mode() {
		return Math.IEEEremainder(lambda, 1.0) == 0.0 ?
				lambda - 1 : Math.floor(lambda);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#median()
	 */
	public double median() {
		return Math.floor(lambda + 1.0 / 3.0 - 0.02 / lambda);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#skewness()
	 */
	public double skewness() {
		return 1.0 / Math.sqrt(lambda);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#kurtosis()
	 */
	public double kurtosis() {
		return 1.0 / lambda;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#f(int)
	 */
	public double f(int n) {
		double r;

		if(n < 0) {
			throw new IllegalArgumentException();
		} else {
			r = Math.log(lambda) * n - Gamma2.lnGamma(n + 1) - lambda;
			return Math.exp(r);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#isInSupport(int)
	 */
	public boolean isInSupport(int n) {
		return n >= 0;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#cdf(double)
	 */
	public double cdf(double x) {
		double l;

		if(x < 0) {
			return 0.0;
		} else {
			l  = Gamma2.lnIncompleteGammaLower(Math.floor(x + 1),
					lambda);
			l -= Gamma2.lnGamma(Math.floor(x + 1));
			return 1 - Math.exp(l);
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
		return (int)Double.doubleToLongBits(lambda);
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof PoissonDistribution) {
			PoissonDistribution d = (PoissonDistribution)o;

			return lambda == d.lambda;
		}
		return false;
	}

}
