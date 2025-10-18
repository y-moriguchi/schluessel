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

import javax.swing.JTree;
import javax.swing.tree.DefaultTreeCellRenderer;

import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.swing.LispLabel;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/17
 */
public class MyLispRenderer extends DefaultTreeCellRenderer {

	//
	private Environment env;
	private Procedure proc;
	private LispMessage mesg;

	/**
	 * 
	 * @param env
	 * @param proc
	 * @param mesg
	 */
	public MyLispRenderer(Environment env, Procedure proc,
			LispMessage mesg) {
		this.env = env;
		this.proc = proc;
		this.mesg = mesg;
	}

	/* (non-Javadoc)
	 * @see javax.swing.tree.DefaultTreeCellRenderer#getTreeCellRendererComponent(javax.swing.JTree, java.lang.Object, boolean, boolean, boolean, int, boolean)
	 */
	@Override
	public Component getTreeCellRendererComponent(JTree tree,
			Object value, boolean sel, boolean expanded, boolean leaf,
			int row, boolean hasFocus) {
		super.getTreeCellRendererComponent(
				tree, value, sel, expanded, leaf,
				row, hasFocus);

		Scheme.callva(proc, env, mesg,
				new LispLabel(this),
				new LispJTree(tree),
				new LispJTreeNode((MyMutableTreeNode)value),
				LispBoolean.getInstance(sel),
				LispBoolean.getInstance(expanded),
				LispBoolean.getInstance(leaf),
				LispBoolean.getInstance(hasFocus));
		return this;
	}

}
