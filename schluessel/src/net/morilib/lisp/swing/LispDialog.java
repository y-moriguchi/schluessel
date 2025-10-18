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
import java.awt.Container;
import java.awt.Window;
import java.awt.event.WindowListener;
import java.util.List;

import javax.swing.JDialog;
import javax.swing.JFrame;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/18
 */
public class LispDialog extends GUIElement
implements ILispComposite, ILispDialog {

	//
	private JDialog dialog;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public abstract static class MakeDialog extends Subr {

		/**
		 * 
		 * @return
		 */
		protected abstract boolean isModal();

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			JDialog res;
			List<Datum> l = LispUtils.consToList(body, mesg);

			if(l.size() != 4) {
				throw mesg.getError("err.argument", body);
			}

			Datum c1a = l.get(0);
			Datum c2a = l.get(1);
			int x = SubrUtils.getSmallInt(l.get(2), mesg);
			int y = SubrUtils.getSmallInt(l.get(3), mesg);
			if(c2a instanceof LispString) {
				if(c1a instanceof ILispWindow) {
					res = new JDialog(
							((ILispWindow)c1a).getFrame(),
							c2a.getString(),
							isModal());
				} else if(c1a instanceof ILispDialog) {
					res = new JDialog(
							((ILispDialog)c1a).getDialog(),
							c2a.getString(),
							isModal());
				} else if(!c1a.isTrue()) {
					res = new JDialog(
							(JFrame)null,
							c2a.getString(),
							isModal());
				} else {
					throw mesg.getError(
							"err.swing.require.parentframeordialog",
							c1a);
				}
			} else {
				throw mesg.getError("err.require.string", c2a);
			}
			res.setSize(x, y);
			return new LispDialog(res);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class MakeModalDialog extends MakeDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispDialog.MakeDialog#isModal()
		 */
		@Override
		protected boolean isModal() {
			return true;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/03/18
	 */
	public static class MakeModelessDialog extends MakeDialog {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.swing.LispDialog.MakeDialog#isModal()
		 */
		@Override
		protected boolean isModal() {
			return false;
		}

	}

	/**
	 * 
	 * @param dialog
	 */
	public LispDialog(JDialog dialog) {
		this.dialog = dialog;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispDialog#getDialog()
	 */
	public JDialog getDialog() {
		return dialog;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComposite#getPane()
	 */
	public Container getPane() {
		return dialog.getContentPane();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#addWindowListener(java.awt.event.WindowListener)
	 */
	public void addWindowListener(WindowListener l) {
		dialog.addWindowListener(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#getAWTWindow()
	 */
	public Window getAWTWindow() {
		return dialog;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return dialog;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<dialog>");
	}

}
