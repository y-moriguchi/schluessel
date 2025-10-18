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

import javax.swing.JButton;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.swing.listener.ActionListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispButton extends LightweightGUIElement
implements ILispButton, ActionListenable {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeButton extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a,
				Environment env, LispMessage mesg) {
			JButton b;

			if(!(c1a instanceof LispString)) {
				throw mesg.getError("err.require.string", c1a);
			} else if(!(c2a instanceof Procedure)) {
				throw mesg.getError("err.require.procedure", c2a);
			}

			b = new JButton(c1a.getString());
			b.addActionListener(LispSwing.createActionListener(
					c2a, env, mesg));
			return new LispButton(b);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/27
	 */
	public static class MakeButtonWithAction extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			JButton b;

			b = new JButton(LispSwing.createAction(
					body, body, env, mesg));
			return new LispButton(b);
		}

	}

	//
	private JButton button;

	/**
	 * 
	 * @param label
	 */
	public LispButton(JButton label) {
		this.button = label;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JButton getComponent() {
		return button;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ActionListenable#addActionListener(java.awt.event.ActionListener)
	 */
	public void addActionListener(ActionListener listener) {
		button.addActionListener(listener);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return getComponent();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<button>");
	}

}
