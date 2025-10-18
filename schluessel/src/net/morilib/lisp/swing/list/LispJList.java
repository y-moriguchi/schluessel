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
package net.morilib.lisp.swing.list;

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JList;
import javax.swing.JScrollPane;

import net.morilib.lisp.swing.LightweightGUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/20
 */
public class LispJList extends LightweightGUIElement {

	//
	JScrollPane component;
	JList list;

	/**
	 * 
	 * @param list
	 */
	public LispJList(JList list) {
		this.list = list;
		this.component = new JScrollPane(list);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispComponent#getComponent()
	 */
	@Override
	public JComponent getComponent() {
		return component;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return component;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<gui-list>");
	}

}
