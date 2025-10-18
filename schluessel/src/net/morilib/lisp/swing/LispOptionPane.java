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

import javax.swing.JOptionPane;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public class LispOptionPane {

	//
	private static final OneToOneSet<Symbol, Integer>
	_MESSAGETYPE = new HashOneToOneSet<Symbol, Integer>(
		new Object[][] {
				new Object[] {
						Symbol.getSymbol("error"),
						JOptionPane.ERROR_MESSAGE
				},
				new Object[] {
						Symbol.getSymbol("information"),
						JOptionPane.INFORMATION_MESSAGE
				},
				new Object[] {
						Symbol.getSymbol("warning"),
						JOptionPane.WARNING_MESSAGE
				},
				new Object[] {
						Symbol.getSymbol("question"),
						JOptionPane.QUESTION_MESSAGE
				},
				new Object[] {
						Symbol.getSymbol("plain"),
						JOptionPane.PLAIN_MESSAGE
				}
		}
	);

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public abstract static class ShowConfirmDialog extends Subr {

		
		protected abstract Datum show(
				Component cm,
				Object mes,
				String title,
				int optionType,
				int type);

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		public Datum eval(
				Datum body, Environment env, LispMessage mesg,
				int optionType) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			Component cm;
			Integer op;

			if(l.size() != 4) {
				throw mesg.getError("err.argument", body);
			} else if(!(l.get(0) instanceof LightweightGUIElement) &&
					l.get(0).isTrue()) {
				throw mesg.getError(
						"err.swing.require.component", l.get(0));
			} else if(!(l.get(2) instanceof LispString)) {
				throw mesg.getError("err.require.string", l.get(2));
			} else if((op = _MESSAGETYPE.getValue(l.get(3))) == null) {
				throw mesg.getError(
						"err.swing.invalidmessagetype", l.get(3));
			}

			cm  = l.get(0).isTrue() ?
					((LightweightGUIElement)l.get(0)).getAWTComponent() : null;
			return show(
					cm,
					LispUtils.print(l.get(1)),
					l.get(2).getString(),
					optionType,
					op);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class ShowYesNoDialog extends ShowConfirmDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return eval(body, env, mesg, JOptionPane.YES_NO_OPTION);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispOptionPane.ShowConfirmDialog#show(javax.swing.JComponent, java.lang.Object, java.lang.String, int)
		 */
		@Override
		protected Datum show(
				Component cm, Object mes, String title,
				int optionType, int type) {
			return toYesNoCancel(JOptionPane.showConfirmDialog(
					cm, mes, title, optionType, type));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class ShowYesNoCancelDialog
	extends ShowConfirmDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return eval(body, env, mesg,
					JOptionPane.YES_NO_CANCEL_OPTION);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispOptionPane.ShowConfirmDialog#show(javax.swing.JComponent, java.lang.Object, java.lang.String, int)
		 */
		@Override
		protected Datum show(
				Component cm, Object mes, String title,
				int optionType, int type) {
			return toYesNoCancel(JOptionPane.showConfirmDialog(
					cm, mes, title, optionType, type));
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class ShowInputDialog extends ShowConfirmDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return eval(body, env, mesg, 0);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispOptionPane.ShowConfirmDialog#show(javax.swing.JComponent, java.lang.Object, java.lang.String, int)
		 */
		@Override
		protected Datum show(
				Component cm, Object mes, String title,
				int optionType, int type) {
			String res = JOptionPane.showInputDialog(
					cm, mes, title, type);

			return (res == null) ?
					LispBoolean.FALSE : new LispString(res);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class ShowMessageDialog extends ShowConfirmDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			return eval(body, env, mesg, 0);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispOptionPane.ShowConfirmDialog#show(javax.swing.JComponent, java.lang.Object, java.lang.String, int)
		 */
		@Override
		protected Datum show(
				Component cm, Object mes, String title,
				int optionType, int type) {
			JOptionPane.showMessageDialog(
					cm, mes, title, type);
			return Undef.UNDEF;
		}

	}

	//
	private static Datum toYesNoCancel(int res) {
		switch(res) {
		case JOptionPane.YES_OPTION:
			return LispBoolean.TRUE;
		case JOptionPane.NO_OPTION:
			return LispBoolean.FALSE;
		case JOptionPane.CANCEL_OPTION:
			return Symbol.getSymbol("canceled");
		default:
			throw new RuntimeException();	
		}
	}

}
