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
package net.morilib.lisp.math.stat.data;

import java.util.Arrays;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.math.stat.dist.ILispDistribution;
import net.morilib.math.stat.StatisticsUtils;
import net.morilib.math.stat.dist.Distribution;
import net.morilib.math.stat.dist.Histogram;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class LispStatisticsData extends Datum2
implements ILispDistribution {

	//
	private double[] data;
	private Histogram histogram;
	private final Distribution distribution = new Distribution() {

		public double f(double x) {
			return histogram.f(x);
		}

		public double cdf(double x) {
			return histogram.cdf(x);
		}

		public double cdf(double x1, double x2) {
			return histogram.cdf(x1, x2);
		}

		public double expectedValue() {
			return StatisticsUtils.mean(data);
		}

		public double variance() {
			return StatisticsUtils.variance(data);
		}

		public double mode() {
			return histogram.mode();
		}

		public double median() {
			return data[data.length / 2];
		}

		public double skewness() {
			return StatisticsUtils.skewness(data);
		}

		public double kurtosis() {
			return StatisticsUtils.kurtosis(data);
		}

	};

	//
	LispStatisticsData(double... vals) {
		data = vals;
		Arrays.sort(data);
		histogram = new Histogram(data);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.analysis.ILispIntegrable1#integrate(net.morilib.lisp.LispReal, net.morilib.lisp.LispReal)
	 */
	public LispReal integrate(LispReal a, LispReal b) {
		return new LispDouble(
				distribution.cdf(a.doubleValue(), b.doubleValue()));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.dist.ILispDistribution#getDistribution()
	 */
	public Distribution getDistribution() {
		return distribution;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<statictics-data>");
	}

}
