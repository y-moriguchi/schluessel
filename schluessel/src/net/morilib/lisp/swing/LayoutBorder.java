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

import java.awt.BorderLayout;
import java.awt.Container;
import java.util.List;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public class LayoutBorder extends Subr {

	//
	private static final OneToOneSet<Symbol, String>
	_DIRECT = new HashOneToOneSet<Symbol, String>(
		new Object[][] {
				new Object[] {
						Symbol.getSymbol("center"),
						BorderLayout.CENTER
				},
				new Object[] {
						Symbol.getSymbol("north"),
						BorderLayout.NORTH
				},
				new Object[] {
						Symbol.getSymbol("south"),
						BorderLayout.SOUTH
				},
				new Object[] {
						Symbol.getSymbol("east"),
						BorderLayout.EAST
				},
				new Object[] {
						Symbol.getSymbol("west"),
						BorderLayout.WEST
				}
		}
	);

	//
	private BorderLayout createbl(
			Container cmp, Datum d0, LispMessage mesg) {
		BorderLayout bl;
		ConsIterator itr;

		bl  = new BorderLayout();
		itr = new ConsIterator(d0);
		while(itr.hasNext()) {
			Datum d = itr.next();

			if(d instanceof Cons) {
				Cons c = (Cons)d;
				String s;

				if(!(c.getCdr() instanceof ILispComponent)) {
					throw mesg.getError(
							"err.swing.require.component", c.getCdr());
				} else if((s = _DIRECT.getValue(c.getCar())) == null) {
					throw mesg.getError(
							"err.swing.invaliddirection", c.getCar());
				}
				cmp.add(((ILispComponent)c.getCdr()).getComponent(), s);
			}
		}
		return bl;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		BorderLayout bl;

		if(l.size() == 2) {
			if(!(l.get(0) instanceof ILispComposite)) {
				throw mesg.getError(
						"err.swing.require.composite", l.get(0));
			}
			bl = createbl(
					((ILispComposite)l.get(0)).getPane(),
					l.get(1), mesg);
		} else if(l.size() == 4) {
			int hg = SubrUtils.getSmallInt(l.get(1), mesg);
			int vg = SubrUtils.getSmallInt(l.get(2), mesg);

			if(!(l.get(0) instanceof ILispComposite)) {
				throw mesg.getError(
						"err.swing.require.composite", l.get(0));
			}
			bl = createbl(
					((ILispComposite)l.get(0)).getPane(),
					l.get(1), mesg);
			bl.setHgap(hg);
			bl.setVgap(vg);
		} else {
			throw mesg.getError("err.argument", body);
		}
		return Undef.UNDEF;
	}

}
