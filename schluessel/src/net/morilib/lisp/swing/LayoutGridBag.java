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

import java.awt.Container;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LayoutGridBag extends BinaryArgs {

	//
	private static final Map<Datum, Integer> _FILL;

	//
	static {
		_FILL = new HashMap<Datum, Integer>();
		_FILL.put(Symbol.getSymbol("none"), GridBagConstraints.NONE);
		_FILL.put(Symbol.getSymbol("horizontal"),
				GridBagConstraints.HORIZONTAL);
		_FILL.put(Symbol.getSymbol("vertical"),
				GridBagConstraints.VERTICAL);
		_FILL.put(Symbol.getSymbol("both"), GridBagConstraints.BOTH);
	}
	
	//
	private int getInt(Map<Symbol, Datum> m,
			String s, int def, LispMessage mesg) {
		Datum d;

		if((d = m.get(Symbol.getSymbol(s))) != null) {
			if(d instanceof LispSmallInt) {
				return d.getInt();
			} else {
				throw mesg.getError("err.require.smallint", d);
			}
		} else {
			return def;
		}
	}

	//
	private double getDouble(Map<Symbol, Datum> m,
			String s, double def, LispMessage mesg) {
		Datum d;

		if((d = m.get(Symbol.getSymbol(s))) != null) {
			if(d instanceof LispReal) {
				return d.getRealDouble();
			} else {
				throw mesg.getError("err.require.smallint", d);
			}
		} else {
			return def;
		}
	}

	//
	private void setConstraints(
			Datum cdr, GridBagConstraints g, LispMessage mesg) {
		Map<Symbol, Datum> m = LispUtils.assocToMapSymbol(cdr);
		Integer d;

		g.gridx = getInt(m, "gridx",
				GridBagConstraints.RELATIVE, mesg);
//		g.gridy = getInt(m, "gridy",
//				GridBagConstraints.RELATIVE, mesg);
		g.gridwidth  = getInt   (m, "gridwidth",  1, mesg);
		g.gridheight = getInt   (m, "gridheight", 1, mesg);
		g.weightx    = getDouble(m, "weightx",    1.0, mesg);
		g.weighty    = getDouble(m, "weighty",    1.0, mesg);
		g.ipadx      = getInt   (m, "ipadx",      0, mesg);
		g.ipady      = getInt   (m, "ipady",      0, mesg);

		d = _FILL.get(m.get(Symbol.getSymbol("fill")));
		g.fill = (d == null) ? GridBagConstraints.NONE : d;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		Container c;
		GridBagConstraints g = new GridBagConstraints();
		ConsIterator itr = new ConsIterator(c2a);

		if(!(c1a instanceof ILispComposite)) {
			throw mesg.getError("err.swing.require.composite", c1a);
		}

		c = ((ILispComposite)c1a).getPane();
		c.setLayout(new GridBagLayout());
//		g.gridy = GridBagConstraints.RELATIVE;
//		g.gridx = GridBagConstraints.RELATIVE;
		g.gridy = 0;
		while(itr.hasNext()) {
			List<Datum> l = LispUtils.consToList(itr.next(), mesg);

			for(int i = 0; i < l.size(); i++) {
				Datum z = l.get(i);
				Datum x;

				if(!(z instanceof Cons)) {
					throw mesg.getError(
							"err.swing.invalidlayout.gridbag", z);
				}

				x = ((Cons)z).getCar();
				if(!(x instanceof ILispComponent)) {
					throw mesg.getError(
							"err.swing.require.component", x);
				}

				setConstraints(((Cons)z).getCdr(), g, mesg);
				if(g.gridx == GridBagConstraints.RELATIVE &&
						i + 1 == l.size()) {
					g.gridwidth = GridBagConstraints.REMAINDER;
				}
				c.add(((ILispComponent)x).getComponent(), g);
			}
			g.gridy++;
		}
		return Undef.UNDEF;
	}

}
