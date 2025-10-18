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

import java.util.List;

import javax.swing.JComponent;
import javax.swing.JMenu;
import javax.swing.JMenuBar;
import javax.swing.JMenuItem;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public class LispMenuBar extends Datum2
implements ILispMenu, JavaObjective {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class MakeMenu extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons) {
				return new LispMenuBar(parseMenuBar(c1a, env, mesg));
			} else {
				throw mesg.getError("err.require.list", c1a);
			}
		}

	}

	//
	private JMenuBar menubar;

	//
	/*package*/ static JComponent parseMenu(
			Datum d, Environment env, LispMessage mesg) {
		if(d instanceof LispMenuItem) {
			return ((LispMenuItem)d).item;
		}

		List<Datum> l = LispUtils.consToList(d, mesg);
		if(l.size() == 2) {
			String t = SubrUtils.getString(l.get(0), mesg);
			ConsIterator itr;

			if(l.get(1) instanceof Procedure) {
				JMenuItem itm = new JMenuItem(t);

				itm.addActionListener(LispSwing.createActionListener(
						l.get(1), env, mesg));
				return itm;
			} else if(l.get(1) instanceof Cons) {
				JMenu men = new JMenu(t);

				itr = new ConsIterator(l.get(1));
				while(itr.hasNext()) {
					Datum z = itr.next();

					if(z.equals(Symbol.getSymbol("separator"))) {
						men.addSeparator();
					} else {
						men.add(parseMenu(z, env, mesg));
					}
				}

				if(!itr.getTerminal().isNil()) {
					throw mesg.getError("err.list", l.get(1));
				}
				return men;
			} else if(!l.get(1).isTrue()) {
				return new JMenuItem(t);
			} else {
				throw mesg.getError("err.swing.invalidmenu", d);
			}
		} else if(l.size() == 4) {
			String t = SubrUtils.getString(l.get(0), mesg);
			JMenuItem m = new JMenuItem(t);

			m.addActionListener(LispSwing.createActionListener(
					l.get(1), env, mesg));
			m.setAccelerator(LispSwing.toKeyStroke(
					l.get(2), l.get(3), mesg));
			return m;
		} else {
			throw mesg.getError("err.swing.invalidmenu", d);
		}
	}

	//
	/*package*/ static JMenuBar parseMenuBar(
			Datum d, Environment env, LispMessage mesg) {
		JMenuBar mb;
		ConsIterator itr;

		mb  = new JMenuBar();
		itr = new ConsIterator(d);
		while(itr.hasNext()) {
			mb.add(parseMenu(itr.next(), env, mesg));
		}

		if(!itr.getTerminal().isNil()) {
			throw mesg.getError("err.list", d);
		}
		return mb;
	}

	/**
	 * 
	 * @param menu
	 */
	public LispMenuBar(JMenuBar menu) {
		this.menubar = menu;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispMenu#getMenuBar()
	 */
	public JMenuBar getMenuBar() {
		return menubar;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<menu>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return menubar;
	}

}
