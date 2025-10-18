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
package net.morilib.lisp.painter.turtle;

import java.awt.image.BufferedImage;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.painter.SchlushFrame;
import net.morilib.lisp.swing.GUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class LispTurtle extends Datum2 {

	//
	SchlushFrame frame;
	TurtleDrawer drawer;

	/**
	 * 
	 * @param frame
	 */
	public LispTurtle(SchlushFrame frame, double x, double y,
			BufferedImage img) {
		this.frame  = frame;
		this.drawer = new TurtleDrawer(x, y, img,
				((GUIElement)frame).getAWTComponent());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<turtle>");
	}

}
