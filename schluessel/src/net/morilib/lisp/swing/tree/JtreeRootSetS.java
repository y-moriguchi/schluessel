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

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/14
 */
public class JtreeRootSetS extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a,
			Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispJTree)) {
			throw mesg.getError("err.swing.require.jtree", c1a);
		} else if(c2a instanceof Cons) {
			LispJTreeUtils.setRoot(
					c1a, LispJTreeUtils.sToNode(c2a), mesg);
			return Undef.UNDEF;
		} else {
			LispJTreeUtils.setRoot(
					c1a, new MyMutableTreeNode(c2a), mesg);
			return Undef.UNDEF;
		}
	}

}
