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
package net.morilib.lisp.painter.drawer;

import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public class CoordinateMap {

	//
	private double x1, y1, x2, y2;

	/**
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 */
	public CoordinateMap(
			double x1, double y1, double x2, double y2) {
		if(x2 <= x1) {
			throw new IllegalArgumentException();
		} else if(y2 <= y1) {
			throw new IllegalArgumentException();
		}

		this.x1 = x1;
		this.y1 = y1;
		this.x2 = x2;
		this.y2 = y2;
	}

	/**
	 * 
	 * @param framex
	 * @param framey
	 * @param x
	 * @param y
	 * @return
	 */
	public MutablePoint2D transform(
			int framex, int framey, double x, double y) {
		double xt = (x - x1) / (x2 - x1);
		double yt = (y - y1) / (y2 - y1);

		return new MutablePoint2D(framex * xt, framey * (1 - yt));
	}

	/**
	 * 
	 * @param framex
	 * @param framey
	 * @param x
	 * @param y
	 * @return
	 */
	public MutablePoint2D transformSize(
			int framex, int framey, double x, double y) {
		double xt = x / (x2 - x1);
		double yt = y / (y2 - y1);

		return new MutablePoint2D(framex * xt, framey * yt);
	}

	/**
	 * 
	 * @param framex
	 * @param framey
	 * @param x
	 * @param y
	 * @return
	 */
	public MutablePoint2D invert(
			int framex, int framey, double xt, double yt) {
		double x = (xt / framex) * (x2 - x1) + x1;
		double y = (1 - (yt / framey)) * (y2 - y1) + y1;

		return new MutablePoint2D(x, y);
	}

	/**
	 * 
	 * @param framex
	 * @param framey
	 * @param xt
	 * @param yt
	 * @return
	 */
	public MutablePoint2D invertSize(
			int framex, int framey, double xt, double yt) {
		double x = (xt / framex) * (x2 - x1);
		double y = (yt / framey) * (y2 - y1);

		return new MutablePoint2D(x, y);
	}

	/**
	 * 
	 * @param framex
	 * @param framey
	 * @param x
	 * @param y
	 * @return
	 */
	public MutablePoint2D normalize(int framex, int framey) {
		return new MutablePoint2D(
				-framex * x1 / (x2 - x1), framey * y2 / (y2 - y1));
	}

}
