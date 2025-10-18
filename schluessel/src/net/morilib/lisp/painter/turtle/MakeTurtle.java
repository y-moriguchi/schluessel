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
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.painter.SchlushFrame;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.GUIElement;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class MakeTurtle extends Subr {

	//
	private static final String CURSOR =
		"/net/morilib/lisp/painter/turtle/default_cursor.png";

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum  c1a = SubrUtils.nextIf(itr, mesg, body);
		double x = SubrUtils.nextDouble(itr, mesg, body);
		double y = SubrUtils.nextDouble(itr, mesg, body);
		Datum  c4a = Iterators.nextIf(itr);
		BufferedImage img = null;
		LispTurtle t;
		File   f;

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			if(c4a != null) {
				f = LispFiles.getFile(env, c4a, mesg);
				img = ImageIO.read(f);
			} else {
				img = ImageIO.read(
						MakeTurtle.class.getResourceAsStream(CURSOR));
			}

			if(c1a instanceof SchlushFrame &&
					c1a instanceof GUIElement) {
				t = new LispTurtle((SchlushFrame)c1a, x, y, img);
				((SchlushFrame)c1a).addDrawer(t.drawer);
				return t;
			} else {
				throw mesg.getError("err.require.frame", c1a);
			}
		} catch (IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
