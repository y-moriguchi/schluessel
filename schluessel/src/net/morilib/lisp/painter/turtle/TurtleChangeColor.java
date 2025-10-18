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

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.painter.SchlushColor;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/28
 */
public class TurtleChangeColor extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c2a instanceof SchlushColor)) {
			throw mesg.getError("err.require.color", c1a);
		} else if(c1a instanceof LispTurtle) {
			((LispTurtle)c1a).drawer.addColor(
					((SchlushColor)c2a).getColor());
			((LispTurtle)c1a).frame.repaint();
			return c1a;
		} else {
			throw mesg.getError("err.turtle.require.turtle", c1a);
		}
	}

}
