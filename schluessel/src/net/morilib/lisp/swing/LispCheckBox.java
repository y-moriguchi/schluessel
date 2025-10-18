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

import javax.swing.AbstractButton;
import javax.swing.JCheckBox;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/19
 */
public class LispCheckBox extends LightweightGUIElement implements ILispButton {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class MakeCheckbox extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispString) {
				return new LispCheckBox(
						new JCheckBox(c1a.getString()));
			} else {
				throw mesg.getError("err.require.string", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class IsCheckboxChecked extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispCheckBox) {
				return LispBoolean.getInstance(
						((LispCheckBox)c1a).checkbox.isSelected());
			} else {
				throw mesg.getError("err.swing.require.checkbox", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/19
	 */
	public static class SetCheckboxCheckedS extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispCheckBox) {
				((LispCheckBox)c1a).checkbox.setSelected(c2a.isTrue());
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.swing.require.checkbox", c1a);
			}
		}

	}

	//
	private JCheckBox checkbox;

	/**
	 * 
	 * @param checkbox
	 */
	public LispCheckBox(JCheckBox checkbox) {
		this.checkbox = checkbox;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return checkbox;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public AbstractButton getComponent() {
		return checkbox;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<checkbox>");
	}

}
