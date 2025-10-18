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
package net.morilib.lisp.painter;

import java.awt.Image;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.painter.drawer.ImageDrawer;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.GUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/18
 */
public class DrawImage extends QuaternaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		Image img;
		File str;

		if(!(c1a instanceof SchlushFrame) &&
				!(c1a instanceof GUIElement)) {
			throw mesg.getError(
					"err.swing.require.canvas", c1a);
		} else if(c2a instanceof ILispImage) {
			img = ((ILispImage)c2a).getImage();
		} else {
			try {
				str = LispFiles.getFile(env, c2a, mesg);
				img = ImageIO.read(str);
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		}
		((SchlushFrame)c1a).addDrawer(new ImageDrawer(
				img,
				SubrUtils.getDouble(c3a, mesg),
				SubrUtils.getDouble(c4a, mesg),
				((GUIElement)c1a).getAWTComponent()));
		return Undef.UNDEF;
	}

}
