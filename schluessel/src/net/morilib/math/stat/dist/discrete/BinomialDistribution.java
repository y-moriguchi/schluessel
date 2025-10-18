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

import java.math.BigInteger;

import net.morilib.lang.Hashes;
import net.morilib.math.special.BetaFunction;
import net.morilib.math.special.Gamma2;
import net.morilib.math.stat.dist.AbstractDiscreteDistribution;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/21
 */
public class BinomialDistribution extends AbstractDiscreteDistribution {

	//
	private BigInteger trial;
	private double succeed;

	/**
	 * @param trial2
	 * @param succeed2
	 */
	public BinomialDistribution(BigInteger trial, double succeed) {
		if(trial == null) {
			throw new NullPointerException();
		} else if(trial.signum() <= 0) {
			throw new IllegalArgumentException();
		} else if(succeed < 0 || succeed > 1) {
			throw new IllegalArgumentException();
		}
		this.trial   = trial;
		this.succeed = succeed;
	}

	/**
	 * @return the trial
	 */
	public BigInteger getTrial() {
		return trial;
	}

	/**
	 * @return the succeed
	 */
	public double getSucceed() {
		return succeed;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#f(int)
	 */
	public double f(int x) {
		double n = trial.doubleValue(), p = (double)succeed;
		double lb, lp, k = (double)x;

		if(!isInSupport(x))  return 0.0;
		lb  = Gamma2.lnGamma(n + 1);
		lb -= Gamma2.lnGamma(k + 1) + Gamma2.lnGamma(n - k + 1);
		lp  = k * Math.log(p) + (n - k) * Math.log(1 - p);
		return Math.exp(lb + lp);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.AbstractDiscreteDistribution#cdf(double)
	 */
	@Override
	public double cdf(double k) {
		double n = trial.doubleValue(), p = (double)succeed;

		if(k < 0) {
			return 0.0;
		} else if(k >= trial.doubleValue()) {
			return 1.0;
		} else {
			return BetaFunction.I(1 - p,
					n - Math.floor(k), 1 + Math.floor(k));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#isInSupport(int)
	 */
	public boolean isInSupport(int n) {
		return n >= 0 && n <= trial.doubleValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#expectedValue()
	 */
	public double expectedValue() {
		return succeed * trial.doubleValue();
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#variance()
	 */
	public double variance() {
		return succeed * trial.doubleValue() * (1 - succeed);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#mode()
	 */
	public double mode() {
		return Math.floor(succeed * (trial.doubleValue() + 1));
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#skewness()
	 */
	public double skewness() {
		double n = trial.doubleValue(), p = (double)succeed;

		return (1 - 2 * p) / Math.sqrt(n * p * (1 - p));
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#kurtosis()
	 */
	public double kurtosis() {
		double n = trial.doubleValue(), p = (double)succeed;

		return (1 - 6 * p * (1 - p)) / (n * p * (1 - p));
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

		r = Hashes.A * (trial.hashCode() + r);
		r = Hashes.A * ((int)Double.doubleToLongBits(succeed) + r);
		return r;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		if(o instanceof BinomialDistribution) {
			BinomialDistribution d = (BinomialDistribution)o;

			return trial.equals(d.trial) && succeed == d.succeed;
		}
		return false;
	}

}
