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
package net.morilib.util.swing;

import javax.swing.JScrollBar;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/21
 */
public class JScrollBar2 extends JScrollBar implements Adjustable2 {

	//
	private static final long serialVersionUID = -2322428520043081281L;

	//
	private double min, max;

	/**
	 * 
	 */
	public JScrollBar2() {
		super();
		this.min = this.max = Double.NaN;
	}

	/**
	 * @param orientation
	 * @param value
	 * @param extent
	 * @param min
	 * @param max
	 */
	public JScrollBar2(
			int orientation, int value, int extent, int min, int max) {
		super(orientation, value, extent, min, max);
		this.min = this.max = Double.NaN;
	}

	/**
	 * @param orientation
	 */
	public JScrollBar2(int orientation) {
		super(orientation);
		this.min = this.max = Double.NaN;
	}

	/**
	 * @param orientation
	 * @param value
	 * @param extent
	 * @param min2
	 * @param max2
	 * @param dmin
	 * @param dmax
	 */
	public JScrollBar2(
			int orientation, int value, int extent, int min,
			int max, double dmin, double dmax) {
		super(orientation, value, extent, min, max);
		this.max = dmax;
		this.min = dmin;
	}

	/**
	 * 
	 * @return
	 */
	public boolean isRealValued() {
		return !Double.isNaN(min);
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.swing.Adjustable2#getRealValue()
	 */
	public double getRealValue() {
		return getRelativeValue() * (max - min) + min;
	}

	/* (non-Javadoc)
	 * @see net.morilib.util.swing.Adjustable2#getRelativeValue()
	 */
	public double getRelativeValue() {
		return ((double)(getValue()   - getMinimum()) /
				(double)(getMaximum() - getMinimum()));
	}

}
