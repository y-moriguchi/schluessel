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

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public class RectangleFiller extends PointSizeDrawer {

	/**
	 * 
	 * @param x1
	 * @param y1
	 * @param x2
	 * @param y2
	 */
	public RectangleFiller(
			double x1, double y1, double x2, double y2) {
		super(x1, y1, x2, y2);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.painter.drawer.Point2Drawer#draw1(int, int, int, int)
	 */
	@Override
	public void draw1(Graphics g, int x1, int y1, int x2, int y2) {
		int xx = Math.max(x1, x2), yx = Math.max(y1, y2);
		int xn = Math.min(x1, x2), yn = Math.min(y1, y2);

		g.fillRect(xn, yn, xx, yx);
	}

}
