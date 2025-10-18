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
public class StringDrawer implements Drawer {

	//
	private String str;
	private double x, y;

	/**
	 * 
	 * @param str
	 * @param x
	 * @param y
	 */
	public StringDrawer(String str, double x, double y) {
		this.str = str;
		this.x   = x;
		this.y   = y;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics, int, int, net.morilib.lisp.painter.drawer.CoordinateMap)
	 */
	public void draw(
			Graphics g, int framex, int framey,
			CoordinateMap coordinate) {
		if(coordinate != null) {
			MutablePoint2D p1 = coordinate.transform(framex, framey, x, y);
			
			g.drawString(
					str,
					p1.getXInt(), p1.getYInt());
		} else {
			g.drawString(str, (int)x, (int)y);
		}
	}

}
