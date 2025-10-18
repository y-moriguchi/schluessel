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

import javax.swing.JTree;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.TreePath;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public class MyTreeSelectionListener implements TreeSelectionListener {

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
	public MyTreeSelectionListener(Environment env, Procedure proc,
			LispMessage mesg) {
		this.env = env;
		this.proc = proc;
		this.mesg = mesg;
	}

	/* (non-Javadoc)
	 * @see javax.swing.event.TreeSelectionListener#valueChanged(javax.swing.event.TreeSelectionEvent)
	 */
	@Override
	public void valueChanged(TreeSelectionEvent e) {
		TreePath[] ps = e.getPaths();
		ConsListBuilder b = new ConsListBuilder();
		MyMutableTreeNode n;

		for(int i = 0; i < ps.length; i++) {
			n = (MyMutableTreeNode)ps[i].getLastPathComponent();
			b.append(new Cons(
					new LispJTreeNode(n),
					LispBoolean.getInstance(e.isAddedPath(i))));
		}

		Scheme.callva(proc, env, mesg,
				new LispJTree((JTree)e.getSource()),
				b.get(),
				LispJTreeUtils.pathToS(e.getOldLeadSelectionPath()),
				LispJTreeUtils.pathToS(e.getNewLeadSelectionPath()));
	}

}
