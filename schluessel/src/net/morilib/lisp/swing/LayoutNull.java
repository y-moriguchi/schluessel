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

import java.awt.Component;
import java.awt.Container;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/07
 */
public class LayoutNull extends BinaryArgs {

	//
	private Component nextIfComponent(ConsIterator jtr,
			LispMessage mesg, Datum b) {
		Datum d = Iterators.nextIf(jtr);

		if(d == null) {
			throw mesg.getError("err.swing.invalidlayout.null", b);
		} else if(d instanceof ILispComponent) {
			return ((ILispComponent)d).getComponent();
		} else {
			throw mesg.getError("err.swing.invalidlayout.null", b);
		}
	}

	//
	private int nextIfInt(ConsIterator jtr, LispMessage mesg,
			Datum b) {
		Datum d = Iterators.nextIf(jtr);

		if(d == null) {
			throw mesg.getError("err.swing.invalidlayout.null", b);
		} else if(d instanceof LispSmallInt) {
			return d.getInt();
		} else {
			throw mesg.getError("err.swing.invalidlayout.null", b);
		}
	}

	//
	private void parseArg(Container c, Datum c2a, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(c2a), jtr;
		Component cm;
		int px, py, sx, sy;

		while(itr.hasNext()) {
			jtr = new ConsIterator(itr.next());
			cm  = nextIfComponent(jtr, mesg, c2a);
			px  = nextIfInt(jtr, mesg, c2a);
			py  = nextIfInt(jtr, mesg, c2a);
			sx  = nextIfInt(jtr, mesg, c2a);
			sy  = nextIfInt(jtr, mesg, c2a);
			SubrUtils.checkProper(jtr, c2a, mesg);
			cm.setBounds(px, py, sx, sy);
			c.add(cm);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof ILispComposite)) {
			throw mesg.getError("err.swing.require.composite", c1a);
		} else {
			((ILispComposite)c1a).getPane().setLayout(null);
			parseArg(((ILispComposite)c1a).getPane(), c2a, mesg);
			return Undef.UNDEF;
		}
	}

}
