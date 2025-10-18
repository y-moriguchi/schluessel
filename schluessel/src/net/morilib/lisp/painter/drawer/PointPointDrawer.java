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

import java.awt.Graphics;

import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public abstract class PointPointDrawer implements Drawer {

	//
	private double x1, y1, x2, y2;
	
	/**
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 */
	public PointPointDrawer(
			double x1, double y1, double x2, double y2) {
		this.x1 = x1;
		this.y1 = y1;
		this.x2 = x2;
		this.y2 = y2;
	}
	
	/**
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 */
	public abstract void draw1(
			Graphics g, int x1, int y1, int x2, int y2);
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics)
	 */
	public void draw(
			Graphics g,
			int framex,
			int framey,
			CoordinateMap coordinate) {
		if(coordinate != null) {
			MutablePoint2D p1 = coordinate.transform(framex, framey, x1, y1);
			MutablePoint2D p2 = coordinate.transform(framex, framey, x2, y2);
			
			draw1(g, p1.getXInt(), p1.getYInt(),
					p2.getXInt(), p2.getYInt());
		} else {
			draw1(g, (int)x1, (int)y1, (int)x2, (int)y2);
		}
	}

}
