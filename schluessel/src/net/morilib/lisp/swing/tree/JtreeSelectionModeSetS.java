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

import javax.swing.tree.TreeSelectionModel;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/18
 */
public class JtreeSelectionModeSetS extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		TreeSelectionModel m;

		if(c1a instanceof LispJTree) {
			m = ((LispJTree)c1a).tree.getSelectionModel();
			if(c2a.equals(JtreeSelectionMode.SINGLE)) {
				m.setSelectionMode(
						TreeSelectionModel.SINGLE_TREE_SELECTION);
			} else if(c2a.equals(JtreeSelectionMode.CONTIGUOUS)) {
				m.setSelectionMode(
						TreeSelectionModel.CONTIGUOUS_TREE_SELECTION);
			} else if(c2a.equals(JtreeSelectionMode.MULTIPLE)) {
				m.setSelectionMode(
						TreeSelectionModel
						.DISCONTIGUOUS_TREE_SELECTION);
			} else {
				throw mesg.getError(
						"err.swing.jtree.selection.mode.invalid",
						c2a);
			}
			return Undef.UNDEF;
		} else {
			throw mesg.getError("err.swing.require.jtree", c1a);
		}
	}

}
