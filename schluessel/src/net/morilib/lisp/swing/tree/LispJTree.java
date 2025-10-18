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
package net.morilib.lisp.swing.tree;

import java.awt.Component;

import javax.swing.JComponent;
import javax.swing.JTree;

import net.morilib.lisp.Datum;
import net.morilib.lisp.swing.ILispComponent;
import net.morilib.lisp.swing.LightweightGUIElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/14
 */
public class LispJTree extends LightweightGUIElement
implements ILispComponent {

	//
	JTree tree;

	/**
	 * 
	 * @param tree
	 */
	public LispJTree(JTree tree) {
		this.tree = tree;
	}

	/**
	 * 
	 * @param root
	 */
	public LispJTree(Datum root) {
		this.tree = new JTree(LispJTreeUtils.sToNode(root));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.GUIElement#getAWTComponent()
	 */
	@Override
	public Component getAWTComponent() {
		return tree;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.swing.ILispComponent#getComponent()
	 */
	@Override
	public JComponent getComponent() {
		return tree;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<gui-tree>");
	}

}
