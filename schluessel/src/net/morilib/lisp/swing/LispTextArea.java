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
import java.util.List;

import javax.swing.JTextArea;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispTextArea extends LightweightGUIElement
implements ILispPrintableComponent, ILispSavableComponent,
ILispLoadableComponent, HasText {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeTextArea extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			JTextArea f;

			if(l.size() == 0) {
				f = new JTextArea();
			} else if(l.size() == 2) {
				int row, col;

				row = SubrUtils.getSmallInt(l.get(0), mesg);
				col = SubrUtils.getSmallInt(l.get(1), mesg);
				f = new JTextArea(row, col);
			} else {
				throw mesg.getError("err.argument", body);
			}
			return new LispTextArea(f);
		}

	}

	//
	private JTextArea area;

	/**
	 * 
	 * @param a
	 */
	public LispTextArea(JTextArea a) {
		this.area = a;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JTextArea getComponent() {
		return area;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#getText()
	 */
	public String getText() {
		return area.getText();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.HasText#setText(java.lang.String)
	 */
	public void setText(String s) {
		area.setText(s);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return getComponent();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispImportableComponent#importFile(java.io.File)
	 */
	@Override
	public void load(File f, String enc) throws IOException {
		area.setText(IOs.toString(f, enc));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispExportableComponent#exportFile(java.io.File)
	 */
	@Override
	public void save(File f, String enc) throws IOException {
		IOs.fromString(f, area.getText(), enc);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<text-area>");
	}

}
