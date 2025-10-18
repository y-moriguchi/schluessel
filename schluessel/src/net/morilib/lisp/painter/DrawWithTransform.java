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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Undef;
import net.morilib.lisp.math.geometry.g2d.ILispAffineTransform2D;
import net.morilib.lisp.painter.drawer.TransformDrawer;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.swing.GUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class DrawWithTransform extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		TransformDrawer dr;

		if(!(c1a instanceof SchlushFrame) &&
				!(c1a instanceof GUIElement)) {
			throw mesg.getError("err.swing.require.canvas", c1a);
		} else if(!(c2a instanceof ILispAffineTransform2D)) {
			throw mesg.getError("err.math.require.affine2d", c2a);
		} else if(!(c3a instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", c3a);
		} else {
			dr = new TransformDrawer(
					((ILispAffineTransform2D)c2a).toAWTTransform());
			((SchlushFrame)c1a).addDrawer(dr);
			Scheme.call((Procedure)c3a, env, mesg, Nil.NIL);
			((SchlushFrame)c1a).addDrawer(dr.getResumeDrawer());
			return Undef.UNDEF;
		}
	}

}
