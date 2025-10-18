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
import java.awt.Image;
import java.awt.image.ImageObserver;

import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/26
 */
public class ImageDrawer implements Drawer {

	//
	private Image img;
	private double x, y;
	private ImageObserver obs;

	/**
	 * 
	 * @param str
	 * @param x
	 * @param y
	 */
	public ImageDrawer(
			Image img, double x, double y, ImageObserver obs) {
		this.img = img;
		this.x   = x;
		this.y   = y;
		this.obs = obs;
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics, int, int, net.morilib.lisp.painter.drawer.CoordinateMap)
	 */
	public void draw(
			Graphics g, int framex, int framey,
			CoordinateMap coordinate) {
		if(coordinate != null) {
			MutablePoint2D p1 =
				coordinate.transform(framex, framey, x, y);
			
			g.drawImage(
					img, p1.getXInt(), p1.getYInt(), obs);
		} else {
			g.drawImage(img, (int)x, (int)y, obs);
		}
	}

}
