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
import java.util.List;

import javax.swing.JTextField;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispSmallInt;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispTextField extends LightweightGUIElement
implements ILispComponent, HasText {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeTextField extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			int size;
			String text = null;
			JTextField f;

			if(l.size() < 1 || l.size() > 2) {
				throw mesg.getError("err.argument", body);
			} else if(l.get(0) instanceof LispSmallInt) {
				size = l.get(0).getInt();
			} else {
				throw mesg.getError(
						"err.require.smallint", l.get(0));
			}

			if(l.size() == 2) {
				if(l.get(1) instanceof LispString) {
					text = l.get(1).getString();
				} else {
					throw mesg.getError(
							"err.require.string", l.get(1));
				}
			}

			f = (text == null) ?
					new JTextField(size) : new JTextField(text, size);
			return new LispTextField(f);
		}

	}

	//
	private JTextField field;

	/**
	 * 
	 * @param f
	 */
	public LispTextField(JTextField f) {
		this.field = f;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JTextField getComponent() {
		return field;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#getText()
	 */
	public String getText() {
		return field.getText();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#setText(java.lang.String)
	 */
	public void setText(String s) {
		field.setText(s);
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
		buf.append("#<text-field>");
	}

}
