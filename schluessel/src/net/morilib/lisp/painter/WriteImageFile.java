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

import java.io.File;
import java.io.IOException;
import java.util.List;

import javax.imageio.ImageIO;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispIOException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.OutputPort;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/12/19
 */
public class WriteImageFile extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		String fname = null, imgf;
		OutputPort outp = null;

		if(l.size() == 2) {
			if(!(l.get(0) instanceof SchlushPainterFrame)) {
				throw mesg.getError(
						"err.require.painterframe", l.get(0));
			}
//			LispUtils.checkString(l.get(1), mesg);
		} else if(l.size() == 3) {
			if(!(l.get(0) instanceof SchlushPainterFrame)) {
				throw mesg.getError(
						"err.require.painterframe", l.get(0));
			}
//			LispUtils.checkString(l.get(1), mesg);
			LispUtils.checkSymbol(l.get(2), mesg);
		}

		if(l.get(1) instanceof LispString) {
			fname = ((LispString)l.get(1)).getString();
		} else if(l.get(1) instanceof OutputPort) {
			outp  = (OutputPort)l.get(1);
		} else {
			throw mesg.getError("err.image.require.filenameoroport",
					l.get(1));
		}

		if(l.size() == 3) {
			imgf = ((Symbol)l.get(2)).getName();
		} else if(fname != null) {
			int ind = fname.lastIndexOf('.');

			if(ind < 0) {
				throw mesg.getError("err.invaild.formatname", "");
			}
			imgf = fname.substring(ind + 1);
		} else {
			throw mesg.getError("err.invaild.formatname");
		}

		try {
			boolean r;

			if(fname != null) {
				r = ImageIO.write(
						((SchlushPainterFrame)l.get(0)).image,
						imgf,
						new File(fname));
			} else {
				r = ImageIO.write(
						((SchlushPainterFrame)l.get(0)).image,
						imgf,
						outp.getPrintStream());
			}

			if(!r) {
				throw mesg.getError("err.invalid.formatname", imgf);
			}
		} catch (IOException e) {
			throw new LispIOException(e);
		}
		return Undef.UNDEF;
	}

}
