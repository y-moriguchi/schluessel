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
package net.morilib.lisp;

import net.morilib.lang.Hashes;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/03
 */
public class LispRangedDouble extends LispDouble
implements java.io.Serializable {

	//
	/*package*/ double error;

	/**
	 * @param x
	 */
	/*package*/ LispRangedDouble(double x, double error) {
		super(trim(x, error));
		this.error = Double.parseDouble(String.format("%.3g", error));
	}

	//
	private static double trim(double x, double err) {
		int d = 1;

		for(int i = 1; i < Math.abs(x / err); i *= 10, d++);
		return Double.parseDouble(String.format("%." + d + "g", x));
	}

	/**
	 * 
	 * @param x
	 * @param error
	 * @return
	 */
	public static LispDouble valueOf(double x, double error) {
		if(error == 0) {
			return new LispDouble(x);
		} else {
			return new LispRangedDouble(x, Math.abs(error));
		}
	}

	/**
	 * 
	 * @return
	 */
	public double getMaxDouble() {
		return number + error;
	}

	/**
	 * 
	 * @return
	 */
	public double getMinDouble() {
		return number - error;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isZeroIncluded() {
		return ((getMaxDouble() > 0 && getMinDouble() < 0) ||
				getMaxDouble() == 0 || getMinDouble() == 0);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Addable#multiply(int)
	 */
	public LispReal multiply(int n) {
		return new LispRangedDouble(number * n, error * n);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Multipliable#power(int)
	 */
	public LispReal power(int n) {
		double min, max;

		if(n < 0) {
			throw new IllegalArgumentException();
		} else if(n == 0) {
			return LispDouble.ONE;
		} else if(n == 1) {
			return this;
		} else if(number - error >= 0) {
			min = Math.pow(number - error, n);
			max = Math.pow(number + error, n);
			return valueOf((max + min) / 2, (max - min) / 2);
		} else if(n % 2 == 1) {
			min = Math.pow(number - error, n);
			max = Math.pow(number + error, n);
			for(int i = 0; i < n; i++) {
				min *= min;  max *= max;
			}
			return valueOf((max + min) / 2, (max - min) / 2);
		} else if(number + error >= 0) {
			max = Math.pow(
					Math.max(error - number, number + error), n);
			return valueOf(max / 2, max / 2);
		} else {
			min = Math.pow(-(number + error), n);
			max = Math.pow(-(number - error), n);
			return valueOf((max + min) / 2, (max - min) / 2);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.algebra.Calculatable#invert()
	 */
	public LispReal invert() {
		double min, max;

		if(number - error > 0) {
			min = 1.0 / (number + error);
			max = 1.0 / (number - error);
			return valueOf((max + min) / 2, (max - min) / 2);
		} else if(number - error == 0) {
			return LispDouble.NaN;
		} else if(number + error >  0) {
			return LispDouble.NaN;
		} else if(number + error == 0) {
			return LispDouble.NaN;
		} else {
			min = 1.0 / (-(number - error));
			max = 1.0 / (-(number + error));
			return valueOf((max + min) / 2, (max - min) / 2);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#uminus()
	 */
	@Override
	public LispRangedDouble uminus() {
		return new LispRangedDouble(-number, error);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#getResult()
	 */
	@Override
	public String getResult() {
		int d = 1;

		if(Double.isNaN(number) || Double.isInfinite(number)) {
			return disp(number);
		} else if(number == 0) {
			return String.format("0.0+-%.3g", error);
		} else {
			for(int i = 1; i < Math.abs(number / error); i *= 10, d++);
			return String.format("%." + d + "g+-%.3g", number, error);
		}
//		return disp(number) + "+-" + disp(error);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#hashCode()
	 */
	@Override
	public int hashCode() {
		int h = Hashes.INIT;

		h = Hashes.A * ((int)Double.doubleToLongBits(number) + h);
		h = Hashes.A * ((int)Double.doubleToLongBits(error)  + h);
		return h;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispReal#equals(java.lang.Object)
	 */
	@Override
	public boolean equals(Object o) {
		return (o instanceof LispRangedDouble &&
				number == ((LispRangedDouble)o).number &&
				error  == ((LispRangedDouble)o).error);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#add(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber add(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return valueOf(
					number + ((LispRangedDouble)x).number,
					Math.max(error, ((LispRangedDouble)x).error));
		} else if(x instanceof LispReal) {
			return valueOf(number + x.getRealDouble(), error);
		} else {
			return super.add(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#sub(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber sub(LispNumber x) {
		if(x instanceof LispRangedDouble) {
			return valueOf(
					number - ((LispRangedDouble)x).number,
					Math.max(error, ((LispRangedDouble)x).error));
		} else if(x instanceof LispReal) {
			return valueOf(number - x.getRealDouble(), error);
		} else {
			return super.sub(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#mul(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber mul(LispNumber x) {
		double y0, y1, y2, y3, y4, ya, yb, yc, yd, mx, mn;
		LispRangedDouble x0;

		if(x instanceof LispRangedDouble) {
			x0 = (LispRangedDouble)x;
			y0 = number * x0.number;
			y1 = y0 + number * x0.error + x0.number * error;
			y2 = y0 - number * x0.error + x0.number * error;
			y3 = y0 + number * x0.error - x0.number * error;
			y4 = y0 - number * x0.error - x0.number * error;
			ya = Math.max(y1, y2);  yc = Math.min(y1, y2);
			yb = Math.max(y3, y4);  yd = Math.min(y3, y4);
			mx = Math.max(ya, yb);  mn = Math.min(yc, yd);
			return valueOf((mx + mn) / 2, (mx - mn) / 2);
		} else if(x instanceof LispReal) {
			return valueOf(number * x.getRealDouble(),
					error * x.getRealDouble());
		} else {
			return super.mul(x);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.LispNumber#div(net.morilib.lisp.LispNumber)
	 */
	@Override
	public LispNumber div(LispNumber x) {
		double y0, y1, y2, y3, y4, ya, yb, yc, yd, mx, mn;
		LispRangedDouble x0;

		if(x instanceof LispRangedDouble) {
			x0 = (LispRangedDouble)x;
			if(x0.isZeroIncluded()) {
				return LispDouble.NaN;
			}
			y0 = number / x0.number;
			y1 = y0 + number * x0.error + x0.number * error;
			y2 = y0 - number * x0.error + x0.number * error;
			y3 = y0 + number * x0.error - x0.number * error;
			y4 = y0 - number * x0.error - x0.number * error;
			ya = Math.max(y1, y2);  yc = Math.min(y1, y2);
			yb = Math.max(y3, y4);  yd = Math.min(y3, y4);
			mx = Math.max(ya, yb);  mn = Math.min(yc, yd);
			return valueOf((mx + mn) / 2, (mx - mn) / 2);
		} else if(x instanceof LispReal) {
			if(x.getRealDouble() == 0) {
				return LispDouble.NaN;
			}
			y1 = (number + error) / x.getRealDouble();
			y2 = (number - error) / x.getRealDouble();
			return valueOf((y1 + y2) / 2.0, Math.abs(y1 - y2) / 2.0);
		} else {
			return super.sub(x);
		}
	}

}
