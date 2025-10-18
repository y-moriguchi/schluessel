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
package net.morilib.math.stat;

import java.math.BigInteger;
import java.util.Arrays;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/17
 */
public final class StatisticsUtils {

	//
	private StatisticsUtils() {}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double sum(double... vals) {
		double r = 0.0;

		for(int i = 0; i < vals.length; i++) {
			r += vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static long sum(int... vals) {
		long r = 0;

		for(int i = 0; i < vals.length; i++) {
			r += vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double mean(double... vals) {
		return sum(vals) / vals.length;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double geometricMean(double... vals) {
		double r = 0.0;

		for(int i = 0; i < vals.length; i++) {
			r += Math.log(vals[i]);
		}
		return Math.exp(r / vals.length);
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double harmonicMean(double... vals) {
		double r = 0.0;

		for(int i = 0; i < vals.length; i++) {
			r += 1.0 / vals[i];
		}
		return vals.length / r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double median(double... vals) {
		double[] ta = new double[vals.length];

		System.arraycopy(vals, 0, ta, 0, vals.length);
		Arrays.sort(ta);
		return ta[vals.length / 2];
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static BigInteger median(BigInteger... vals) {
		BigInteger[] ta = new BigInteger[vals.length];

		System.arraycopy(vals, 0, ta, 0, vals.length);
		Arrays.sort(ta);
		return ta[vals.length / 2];
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double max(double... vals) {
		double r = Double.MIN_VALUE;

		for(int i = 0; i < vals.length; i++) {
			if(vals[i] > r)  r = vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static int max(int... vals) {
		int r = Integer.MIN_VALUE;

		for(int i = 0; i < vals.length; i++) {
			if(vals[i] > r)  r = vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static int whereIsMax(int... vals) {
		int r = Integer.MIN_VALUE, v = -1;

		for(int i = 0; i < vals.length; i++) {
			if(vals[i] > r) {
				r = vals[i];  v = i;
			}
		}
		return v;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double min(double... vals) {
		double r = Double.MAX_VALUE;

		for(int i = 0; i < vals.length; i++) {
			if(vals[i] < r)  r = vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static int min(int... vals) {
		int r = Integer.MAX_VALUE;

		for(int i = 0; i < vals.length; i++) {
			if(vals[i] < r)  r = vals[i];
		}
		return r;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double mode(double... vals) {
		double max = max(vals), min = min(vals);
		int[] hist = toHistogram(vals);

		return mode(min, max, hist);
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static BigInteger mode(BigInteger... vals) {
		BigInteger[] ta = new BigInteger[vals.length];
		BigInteger p = null, m = p, v;
		int r = 0, c = 0;

		System.arraycopy(vals, 0, ta, 0, vals.length);
		Arrays.sort(ta);
		for(int i = 0; i < vals.length; i++) {
			v = ta[i];
			if(p == null || p != v) {
				p = v;
				if(c > r) {
					r = c;  m = p;
				}
				c = 1;
			} else {
				c++;
			}
		}
		return m;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static int[] toHistogram(double... vals) {
		int k = (int)Math.sqrt(vals.length);
		int[] r = new int[k + 2];
		double max = max(vals), min = min(vals);
		double h;

		if(vals.length == 0) {
			throw new IllegalArgumentException();
		}

		Arrays.fill(r, 0);
		h = (max - min) / k;
		for(int i = 0; i < vals.length; i++) {
			if(vals[i] == max) {
				r[k]++;
			} else if(vals[i] > max) {
				r[k + 1]++;
			} else if(vals[i] < min) {
				r[0]++;
			} else {
				r[(int)((vals[i] - min) / h) + 1]++;
			}
		}
		return r;
	}

	/**
	 * 
	 * @param min
	 * @param max
	 * @param histogram
	 * @return
	 */
	public static double mode(double min, double max,
			int... histogram) {
		int p = whereIsMax(histogram);
		double q = (double)(p - 0.5) / (histogram.length - 2);

		if(histogram.length < 2) {
			throw new IllegalArgumentException();
		} else if(p == 0) {
			return Double.NEGATIVE_INFINITY;
		} else if(p == histogram.length) {
			return Double.POSITIVE_INFINITY;
		} else {
			return (max - min) * q + min;
		}
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double variance(double... vals) {
		double r = 0.0, m = mean(vals);

		for(int i = 0; i < vals.length; i++) {
			r += (vals[i] - m) * (vals[i] - m);
		}
		return r / vals.length;
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double unbiasedVariance(double... vals) {
		double r = 0.0, m = mean(vals);

		for(int i = 0; i < vals.length; i++) {
			r += (vals[i] - m) * (vals[i] - m);
		}
		return r / (vals.length - 1);
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double skewness(double... vals) {
		double r = 0.0, m = mean(vals), s = variance(vals);

		for(int i = 0; i < vals.length; i++) {
			r += (vals[i] - m) * (vals[i] - m) * (vals[i] - m);
		}
		return r / vals.length / Math.pow(s, 1.5);
	}

	/**
	 * 
	 * @param vals
	 * @return
	 */
	public static double kurtosis(double... vals) {
		double r = 0.0, m = mean(vals), s = variance(vals);

		for(int i = 0; i < vals.length; i++) {
			r += ((vals[i] - m) * (vals[i] - m) *
					(vals[i] - m) * (vals[i] - m));
		}
		return r / vals.length / s / s - 3.0;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double covariance(double[] x, double[] y) {
		double r = 0.0, mx = mean(x), my = mean(y);

		if(x == null || y == null) {
			throw new NullPointerException();
		} else if(x.length != y.length) {
			throw new IllegalArgumentException();
		} else if(x.length == 0) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < x.length; i++) {
			r += (x[i] - mx) * (y[i] - my);
		}
		return r / x.length;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double correlation(double[] x, double[] y) {
		double r = 0.0, xs = 0.0, ys = 0.0;
		double mx = mean(x), my = mean(y);

		if(x == null || y == null) {
			throw new NullPointerException();
		} else if(x.length != y.length) {
			throw new IllegalArgumentException();
		} else if(x.length == 0) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < x.length; i++) {
			r  += (x[i] - mx) * (y[i] - my);
			xs += (x[i] - mx) * (x[i] - mx);
			ys += (y[i] - my) * (y[i] - my);
		}
		return r / Math.sqrt(xs) / Math.sqrt(ys);
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static double[] regression(double[] x, double[] y) {
		double x11 = 0.0, x12 = 0.0, x21 = 0.0, x22 = 0.0;
		double y1  = 0.0, y2  = 0.0;
		double bt0, bt1, det;

		if(x == null || y == null) {
			throw new NullPointerException();
		} else if(x.length != y.length) {
			throw new IllegalArgumentException();
		} else if(x.length == 0) {
			throw new IllegalArgumentException();
		}

		for(int i = 0; i < x.length; i++) {
			x11 += 1;
			x12 += x[i];
			x21 += x[i];
			x22 += x[i] * x[i];
			y1  += y[i];
			y2  += x[i] * y[i];
		}
		det  = x11 * x22 - x12 * x21;
		bt0  = (x11 *  y2 - x21 *  y1) / det;
		bt1  = (y1  * x22 - y2  * x12) / det;
		return new double[] { bt0, bt1 };
	}

}
