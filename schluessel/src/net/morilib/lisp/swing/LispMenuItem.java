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

import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.JMenuItem;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.swing.listener.ActionListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/19
 */
public class LispMenuItem extends Datum2
implements ActionListenable, JavaObjective {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class MakeMenuItem extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			JMenuItem m;

			if(l.size() == 2) {
				String t = SubrUtils.getString(l.get(0), mesg);

				m = new JMenuItem(t);
				m.addActionListener(LispSwing.createActionListener(
						l.get(1), env, mesg));
			} else if(l.size() == 4) {
				String t = SubrUtils.getString(l.get(0), mesg);

				m = new JMenuItem(t);
				m.addActionListener(LispSwing.createActionListener(
						l.get(1), env, mesg));
				m.setAccelerator(LispSwing.toKeyStroke(
						l.get(2), l.get(3), mesg));
			} else {
				throw mesg.getError("err.argument", body);
			}
			return new LispMenuItem(m);
		}

	}

	/**
	 * 
	 * @param item
	 */
	public LispMenuItem(JMenuItem item) {
		this.item = item;
	}

	//
	/*package*/ JMenuItem item;

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ActionListenable#addActionListener(java.awt.event.ActionListener)
	 */
	public void addActionListener(ActionListener listener) {
		item.addActionListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<menu-item>");
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return item;
	}

}
