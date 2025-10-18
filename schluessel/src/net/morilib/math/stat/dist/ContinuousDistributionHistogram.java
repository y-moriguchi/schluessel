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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/18
 */
public class ContinuousDistributionHistogram
extends AbstractDiscreteDistribution {

	//
	private double max, min, dx;
	private ContinuousDistribution dist;

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#cdf(double)
	 */
	public double cdf(double x) {
		double n = Math.floor(x);

		if(n < 0) {
			return 0;
		} else if(n * dx < max - min) {
			return dist.cdf(n * dx + min);
		} else {
			return 1.0;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#expectedValue()
	 */
	public double expectedValue() {
		return dist.expectedValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#variance()
	 */
	public double variance() {
		return dist.variance();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#mode()
	 */
	public double mode() {
		return dist.mode();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#median()
	 */
	public double median() {
		return dist.median();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#skewness()
	 */
	public double skewness() {
		return dist.skewness();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#kurtosis()
	 */
	public double kurtosis() {
		return dist.kurtosis();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#f(int)
	 */
	public double f(int n) {
		if(n < 0) {
			return 0;
		} else if(n * dx < max - min) {
			return dist.cdf(n * dx + min, (n + 1) * dx + min);
		} else {
			return 1.0;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#isInSupport(int)
	 */
	public boolean isInSupport(int n) {
		return n >= 0 && dist.isInSupport(n * dx + min);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#supportMinimum()
	 */
	public int supportMinimum() {
		return 0;
	}

}
