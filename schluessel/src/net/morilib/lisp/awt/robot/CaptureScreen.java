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
package net.morilib.lisp.awt.robot;

import java.awt.AWTException;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.Robot;
import java.awt.image.BufferedImage;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.painter.ILispImage;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/07
 */
public class CaptureScreen extends Subr {

	//
	private static abstract class Img extends Datum2
	implements ILispImage {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<captured-image>");
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1, r1;
		Robot rb;
		int rx, ry, rw, rh;
		final BufferedImage im;
		Img li;

		d1 = SubrUtils.nextIf(itr, mesg, body);
		if(d1 instanceof LispRobot) {
			rb = ((LispRobot)d1).robot;
			r1 = SubrUtils.nextIf(itr, mesg, body);
		} else {
			try {
				rb = new Robot();
				r1 = d1;
			} catch (AWTException e) {
				throw mesg.getError("err.awt.robot.notavailable");
			}
		}

		rx = SubrUtils.getSmallInt(r1, mesg);
		ry = SubrUtils.nextSmallInt(itr, mesg, body);
		rw = SubrUtils.nextSmallInt(itr, mesg, body);
		rh = SubrUtils.nextSmallInt(itr, mesg, body);
		if(rx < 0) {
			throw mesg.getError("err.require.int.nonnegative", rx+"");
		} else if(ry < 0) {
			throw mesg.getError("err.require.int.nonnegative", ry+"");
		} else if(rw <= 0) {
			throw mesg.getError("err.require.int.nonnegative", rw+"");
		} else if(rh <= 0) {
			throw mesg.getError("err.require.int.nonnegative", rh+"");
		}
		im = rb.createScreenCapture(new Rectangle(rx, ry, rw, rh));
		li = new Img() {

			@Override
			public Image getImage() {
				return im;
			}

		};
		return li;
	}

}
