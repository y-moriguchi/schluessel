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

import net.morilib.math.stat.StatisticsUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public class Histogram extends AbstractDiscreteDistribution {

	//
	int[] frequency;
	double min, max, width;
	double mean, variance, mode, median, skewness, kurtosis;
	int sum;

	/**
	 * 
	 * @param vals
	 */
	public Histogram(double... vals) {
		int k = (int)Math.sqrt(vals.length);

		min = StatisticsUtils.min(vals);
		max = StatisticsUtils.min(vals);
		width = (max - min) / k;
		mean = StatisticsUtils.mean(vals);
		mode = StatisticsUtils.mode(vals);
		median = StatisticsUtils.median(vals);
		variance = StatisticsUtils.variance(vals);
		skewness = StatisticsUtils.skewness(vals);
		kurtosis = StatisticsUtils.kurtosis(vals);
		sum = vals.length;
		frequency = StatisticsUtils.toHistogram(vals);
	}

	/**
	 * 
	 * @return
	 */
	public int getBins() {
		return frequency.length;
	}

	/**
	 * 
	 * @param n
	 * @return
	 */
	public int frequency(int n) {
		return frequency[n];
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#f(int)
	 */
	public double f(int n) {
		return frequency[n] / sum;
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
		return mode;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#median()
	 */
	public double median() {
		return median;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#skewness()
	 */
	public double skewness() {
		return skewness;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ProbabilityDistribution#kurtosis()
	 */
	public double kurtosis() {
		return kurtosis;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#isInSupport(int)
	 */
	public boolean isInSupport(int n) {
		return 0 <= n && n < frequency.length;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#supportMinimum()
	 */
	public int supportMinimum() {
		return 0;
	}

}
