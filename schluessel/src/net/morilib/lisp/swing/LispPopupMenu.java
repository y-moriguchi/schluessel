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

import javax.swing.JComponent;
import javax.swing.JPopupMenu;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/15
 */
public class LispPopupMenu extends GUIElement
implements ILispComponent {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/07/15
	 */
	public static class MakePopupMenu extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof Cons) {
				return new LispPopupMenu(parseMenuBar(c1a, env, mesg));
			} else {
				throw mesg.getError("err.require.list", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/07/15
	 */
	public static class ShowPopupMenu extends QuaternaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
				Datum c4a, Environment env, LispMessage mesg) {
			int x = SubrUtils.getSmallInt(c3a, mesg);
			int y = SubrUtils.getSmallInt(c4a, mesg);
			Component c;

			if(!c2a.isTrue()) {
				c = null;
			} else if(c2a instanceof ILispComponent) {
				c = ((ILispComponent)c2a).getComponent();
			} else {
				throw mesg.getError("err.swing.require.component",
						c2a);
			}

			if(c1a instanceof LispPopupMenu) {
				((LispPopupMenu)c1a).menu.show(c, x, y);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.swing.require.popupmenu",
						c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2012/07/15
	 */
	public static class HidePopupMenu extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispPopupMenu) {
				((LispPopupMenu)c1a).menu.setVisible(false);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.swing.require.popupmenu",
						c1a);
			}
		}

	}

	//
	private JPopupMenu menu;

	/**
	 * 
	 * @param menu
	 */
	public LispPopupMenu(JPopupMenu menu) {
		this.menu = menu;
	}

	//
	/*package*/ static JPopupMenu parseMenuBar(
			Datum d, Environment env, LispMessage mesg) {
		JPopupMenu mb;
		ConsIterator itr;

		mb  = new JPopupMenu();
		itr = new ConsIterator(d);
		while(itr.hasNext()) {
			mb.add(LispMenuBar.parseMenu(itr.next(), env, mesg));
		}

		if(!itr.getTerminal().isNil()) {
			throw mesg.getError("err.list", d);
		}
		return mb;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispComponent#getComponent()
	 */
	@Override
	public JComponent getComponent() {
		return menu;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return menu;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<popup-menu>");
	}

}
