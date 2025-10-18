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
import java.io.File;
import java.io.IOException;

import javax.swing.Action;
import javax.swing.JComponent;
import javax.swing.JTextPane;
import javax.swing.KeyStroke;
import javax.swing.text.AttributeSet;
import javax.swing.text.Keymap;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/03
 */
public class LispTextPane extends LightweightGUIElement
implements ILispPrintableComponent, ILispSavableComponent,
ILispLoadableComponent, HasText {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/03
	 */
	public static class MakeTextPane extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);

			SubrUtils.checkTerminated(itr, body, mesg);
			return new LispTextPane(new JTextPane());
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/03
	 */
	public static class AddKeyActionS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum t = SubrUtils.nextIf(itr, mesg, body);
			Datum k = SubrUtils.nextIf(itr, mesg, body);
			Datum c = SubrUtils.nextIf(itr, mesg, body);

			if(t instanceof LispTextPane) {
				Action a;

				a = LispSwing.createAction(
						itr.rest(), body, env, mesg);
				((LispTextPane)t).addKeyAction(
						LispSwing.toKeyStroke(k, c, mesg), a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.textpane", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/03
	 */
	public static class SetDefaultKeyActionS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum t = SubrUtils.nextIf(itr, mesg, body);

			if(t instanceof LispTextPane) {
				Action a;

				a = LispSwing.createAction(
						itr.rest(), body, env, mesg);
				((LispTextPane)t).setDefaultKeyAction(a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.textpane", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/03
	 */
	public static class SetStyleS extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum t = SubrUtils.nextIf(itr, mesg, body);
			int   o = SubrUtils.nextSmallInt(itr, mesg, body);
			int   l = SubrUtils.nextSmallInt(itr, mesg, body);

			if(t instanceof LispTextPane) {
				AttributeSet a;

				a = LispSwing.createAttributeSet(
						itr.rest(), body, env, mesg);
				((LispTextPane)t).setStyle(o, l, a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError(
						"err.swing.require.textpane", body);
			}
		}

	}

	//
	private JTextPane textpane;

	/**
	 * 
	 * @param textpane
	 */
	public LispTextPane(JTextPane textpane) {
		this.textpane = textpane;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return textpane;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#getText()
	 */
	public String getText() {
		return textpane.getText();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#setText(java.lang.String)
	 */
	public void setText(String s) {
		textpane.setText(s);
	}

	/**
	 * 
	 * @param ks
	 * @param a
	 */
	public void addKeyAction(KeyStroke ks, Action a) {
		Keymap km = textpane.getKeymap();

		km.addActionForKeyStroke(ks, a);
	}

	/**
	 * 
	 * @param a
	 */
	public void setDefaultKeyAction(Action a) {
		Keymap km = textpane.getKeymap();

		km.setDefaultAction(a);
	}

	/**
	 * 
	 * @param off
	 * @param len
	 * @param a
	 */
	public void setStyle(int off, int len, AttributeSet a) {
		textpane.getStyledDocument().setCharacterAttributes(
				off, len, a, true);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispComponent#getComponent()
	 */
	@Override
	public JComponent getComponent() {
		return textpane;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispImportableComponent#importFile(java.io.File)
	 */
	@Override
	public void load(File f, String enc) throws IOException {
		textpane.setText(IOs.toString(f, enc));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispExportableComponent#exportFile(java.io.File)
	 */
	@Override
	public void save(File f, String enc) throws IOException {
		IOs.fromString(f, textpane.getText(), enc);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<text-pane>");
	}

}
