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

import javax.swing.JComponent;
import javax.swing.JScrollPane;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/14
 */
public class LispScrollPane extends LightweightGUIElement implements ILispComponent {

	//
	private JScrollPane pane;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/07/14
	 */
	public static class MakeScrollPane extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof ILispComponent) {
				return new LispScrollPane(new JScrollPane(
						((ILispComponent)c1a).getComponent()));
			} else {
				throw mesg.getError("err.swing.require.component",
						c1a);
			}
		}

	}

	/**
	 * 
	 */
	public LispScrollPane(JScrollPane pane) {
		this.pane = pane;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return pane;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return getComponent();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		// TODO Auto-generated method stub

	}

}
