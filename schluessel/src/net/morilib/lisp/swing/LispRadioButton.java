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
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.AbstractButton;
import javax.swing.JRadioButton;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
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
public class LispRadioButton extends LightweightGUIElement
implements ILispButton, ActionListenable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class MakeRadioButton extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			JRadioButton r;

			if(l.size() == 1) {
				String t = SubrUtils.getString(l.get(0), mesg);

				r = new JRadioButton(t);
			} else if(l.size() == 2) {
				String t = SubrUtils.getString(l.get(0), mesg);

				r = new JRadioButton(t);
				r.addActionListener(LispSwing.createActionListener(
						l.get(1), env, mesg));
			} else {
				throw mesg.getError("err.argument", body);
			}
			return new LispRadioButton(r);
		}

	}

	//
	/*package*/ JRadioButton radio;

	/**
	 * 
	 * @param radio
	 */
	public LispRadioButton(JRadioButton radio) {
		this.radio = radio;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return radio;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public AbstractButton getComponent() {
		return radio;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ActionListenable#addActionListener(java.awt.event.ActionListener)
	 */
	public void addActionListener(ActionListener listener) {
		radio.addActionListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<radio-button>");
	}

}
