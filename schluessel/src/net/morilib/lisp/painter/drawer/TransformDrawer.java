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
import java.awt.Graphics2D;
import java.awt.geom.AffineTransform;

import net.morilib.lisp.painter.geom.MutablePoint2D;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class TransformDrawer implements Drawer {

	//
	private AffineTransform tx, resume;

	/**
	 * 
	 * @param tx
	 */
	public TransformDrawer(AffineTransform tx) {
		this.tx = tx;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Drawer#draw(java.awt.Graphics, int, int, net.morilib.lisp.painter.drawer.CoordinateMap)
	 */
	@Override
	public void draw(Graphics g, int framex, int framey,
			CoordinateMap coordinate) {
		MutablePoint2D mp = coordinate.transform(framex, framey, 0, 0);
		AffineTransform ty = new AffineTransform(tx);
		AffineTransform tz = AffineTransform.getTranslateInstance(
				mp.getX(), mp.getY());
		AffineTransform tw = AffineTransform.getTranslateInstance(
				-mp.getX(), -mp.getY());

		resume = ((Graphics2D)g).getTransform();
		tz.concatenate(ty);
		tz.concatenate(tw);
		((Graphics2D)g).setTransform(tz);
	}

	/**
	 * 
	 * @return
	 */
	public Drawer getResumeDrawer() {
		return new Drawer() {

			@Override
			public void draw(Graphics g, int framex, int framey,
					CoordinateMap coordinate) {
				((Graphics2D)g).setTransform(resume);
			}

		};
	}

}
