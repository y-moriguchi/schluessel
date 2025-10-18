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

import java.awt.geom.Rectangle2D;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.math.geometry.g2d.LispF64Vector2D;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/03
 */
public class GetStringBounds extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Datum c3a, Environment env,
			LispMessage mesg) {
		Rectangle2D r;

		if(!(c1a instanceof LispString)) {
			throw mesg.getError("err.require.string", c1a);
		} else if(!(c2a instanceof SchlushFont)) {
			throw mesg.getError("err,require.font", c2a);
		} else if(!(c3a instanceof SchlushFrame)) {
			throw mesg.getError("err.require.frame", c3a);
		}

		r = ((SchlushFrame)c3a).calculateFontBounds(
				c1a.getString(), (SchlushFont)c2a);
		return new LispF64Vector2D(r.getWidth(), r.getHeight());
	}

}
