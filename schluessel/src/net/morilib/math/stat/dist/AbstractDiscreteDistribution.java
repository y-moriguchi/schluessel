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
 * @author MORIGUCHI, Yuichiro 2012/02/14
 */
public abstract class AbstractDiscreteDistribution
implements DiscreteDistribution {

	/* (non-Javadoc)
	 * @see net.morilib.lang.transform.DoubleTransform#f(double)
	 */
	public double f(double x) {
		if(x >= 0.0 && Math.IEEEremainder(x, 1.0) == 0.0) {
			return f((int)x);
		} else {
			return 0.0;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.DiscreteDistribution#cdf(int)
	 */
	public double cdf(double x) {
		double r = 0;
		int n = (int)(x > 0 ? x : 0);

		for(int i = 0; i <= n; i++) {
			r += f(i);
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.ContinuousDistribution#cdf(double, double)
	 */
	public double cdf(double x1, double x2) {
		return cdf(x2) - cdf(x1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.math.stat.dist.Distribution#median()
	 */
	public double median() {
		int r = supportMinimum();

		for(; cdf(r) < 0.5; r++);
		return r;
	}

}
