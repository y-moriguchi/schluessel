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
package net.morilib.lisp.swing.listener;

import java.awt.event.WindowListener;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.swing.LispSwing;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/27
 */
public class WindowListenerAddS extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(
			Datum body, Environment env, LispMessage mesg) {
		if(body instanceof Cons) {
			Cons c = (Cons)body;

			if(c.getCar() instanceof WindowListenable) {
				WindowListener l;

				l = LispSwing.createWindowListener(
						c.getCdr(), body, env, mesg);
				((WindowListenable)c.getCar())
				.addWindowListener(l);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.window",
						c.getCar());
			}
		} else {
			throw mesg.getError("err.argument", body);
		}
	}

}