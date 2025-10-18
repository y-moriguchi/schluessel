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

import net.morilib.lisp.Datum2;
import net.morilib.lisp.math.stat.xy.ILispXYData;
import net.morilib.math.stat.StatisticsUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/25
 */
public class LispStatisticsXYData extends Datum2
implements ILispXYData {

	//
	private double[] dataX, dataY;
	private transient double[] reg = null;

	//
	LispStatisticsXYData(double[] x, double[] y) {
		dataX = x;
		dataY = y;
	}

	/**
	 * 
	 * @param x
	 * @param y
	 * @return
	 */
	public static LispStatisticsXYData newInstance(
			double[] x, double[] y) {
		double[] x1, y1;

		if(x == null || y == null) {
			throw new NullPointerException();
		} else if(x.length == 0 || y.length == 0) {
			throw new IllegalArgumentException();
		} else if(x.length != y.length) {
			throw new IllegalArgumentException();
		}
		x1 = new double[x.length];  y1 = new double[y.length];
		System.arraycopy(x, 0, x1, 0, x.length);
		System.arraycopy(y, 0, y1, 0, y.length);
		return new LispStatisticsXYData(x1, y1);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.ILispXYData#covariance()
	 */
	public double covariance() {
		return StatisticsUtils.covariance(dataX, dataY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.ILispXYData#correlationCoefficient()
	 */
	public double correlationCoefficient() {
		return StatisticsUtils.correlation(dataX, dataY);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.ILispXYData#regression()
	 */
	public double[] regression() {
		if(reg == null) {
			synchronized(this) {
				reg = StatisticsUtils.regression(dataX, dataY);
			}
		}
		return reg;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.ILispXYData#errorTerm()
	 */
	public double[] errorTerm() {
		double[] r = new double[dataX.length];

		regression();
		for(int i = 0; i < dataX.length; i++) {
			r[i] = dataY[i] - dataX[i] * reg[0] - reg[1];
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.stat.xy.ILispXYData#errorTermData()
	 */
	public LispStatisticsData errorTermData() {
		return new LispStatisticsData(errorTerm());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<statistics-xy-data>");
	}

}
