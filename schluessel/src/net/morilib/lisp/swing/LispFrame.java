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

import javax.swing.JFrame;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispFrame extends GUIElement
implements ILispComposite, ILispWindow {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class MakeWindow extends Subr {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> l = LispUtils.consToList(body, mesg);
			JFrame w;
			String t;
			int x, y;

			if(l.size() == 1) {
				t = SubrUtils.getString(l.get(0), mesg);
				w = new JFrame(t);
				return new LispFrame(w);
			} else if(l.size() == 3) {
				t = SubrUtils.getString(l.get(0), mesg);
				x = SubrUtils.getSmallInt(l.get(1), mesg);
				y = SubrUtils.getSmallInt(l.get(2), mesg);
				w = new JFrame(t);
				w.setSize(x, y);
				return new LispFrame(w);
			} else if(l.size() == 4) {
				t = SubrUtils.getString(l.get(0), mesg);
				x = SubrUtils.getSmallInt(l.get(1), mesg);
				y = SubrUtils.getSmallInt(l.get(2), mesg);
				w = new JFrame(t);
				w.setSize(x, y);
				w.setJMenuBar(
						LispMenuBar.parseMenuBar(l.get(3), env, mesg));
				return new LispFrame(w);
			} else {
				throw mesg.getError("err.argument", body);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class ExecWindow extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispFrame) {
				((LispFrame)c1a).window.setDefaultCloseOperation(
						JFrame.EXIT_ON_CLOSE);
				((LispFrame)c1a).window.setVisible(true);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.swing.require.window", c1a);
			}
		}

	}

	//
	private JFrame window;

	/**
	 * 
	 * @param window
	 */
	public LispFrame(JFrame window) {
		this.window = window;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComposite#getPane()
	 */
	public Container getPane() {
		return window.getContentPane();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispFrame#getFrame()
	 */
	public JFrame getFrame() {
		return window;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return window;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#addWindowListener(java.awt.event.WindowListener)
	 */
	public void addWindowListener(WindowListener l) {
		window.addWindowListener(l);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.WindowListenable#getAWTWindow()
	 */
	public Window getAWTWindow() {
		return window;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<window>");
	}

}
