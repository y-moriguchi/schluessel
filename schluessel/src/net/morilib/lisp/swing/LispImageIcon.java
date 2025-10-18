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
package net.morilib.lisp.swing;

import java.io.File;
import java.io.IOException;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/27
 */
public class LispImageIcon extends Datum2
implements ILispIcon, JavaObjective {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/27
	 */
	public static class MakeImageIcon extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			File f;

			try {
				f = LispFiles.getFile(env, c1a, mesg);
				return new LispImageIcon(new ImageIcon(f.toString()));
			} catch (IOException e) {
				throw mesg.getError("err.io");
			}
		}

	}

	//
	private ImageIcon icon;

	/**
	 * 
	 * @param icon
	 */
	public LispImageIcon(ImageIcon icon) {
		this.icon = icon;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispIcon#getIcon()
	 */
	public Icon getIcon() {
		return icon;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<image-icon>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return icon;
	}

}
