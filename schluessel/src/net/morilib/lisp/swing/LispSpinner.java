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
import javax.swing.JSpinner;
import javax.swing.event.ChangeListener;

import net.morilib.lisp.swing.listener.ChangeListenable;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/03/26
 */
public abstract class LispSpinner extends LightweightGUIElement
implements ILispComponent, ChangeListenable {

	/**
	 * 
	 */
	protected JSpinner spinner;

	/**
	 * 
	 * @param spinner
	 */
	protected LispSpinner(JSpinner spinner) {
		this.spinner = spinner;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.LispComponent#getComponent()
	 */
	public JComponent getComponent() {
		return spinner;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return spinner;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ChangeListenable#addChangeListener(javax.swing.event.ChangeListener)
	 */
	public void addChangeListener(ChangeListener listener) {
		spinner.addChangeListener(listener);
	}

}
