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

import java.util.ArrayList;
import java.util.List;

import javax.swing.tree.TreePath;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/14
 */
public class JtreeSelectedPathLists extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		TreePath[] ps;
		List<Datum> l = new ArrayList<Datum>();
		ConsListBuilder b = new ConsListBuilder();

		if(c1a instanceof LispJTree) {
			ps = ((LispJTree)c1a).tree.getSelectionPaths();
			if(ps == null || ps.length == 0)  return LispBoolean.FALSE;
			for(int i = 0; i < ps.length; i++) {
				for(Object o : ps[i].getPath()) {
					l.add((Datum)
							((MyMutableTreeNode)o).getUserObject());
				}
				b.append(LispUtils.listToCons(l));
			}
			return b.get();
		} else {
			throw mesg.getError("err.swing.require.jtree", c1a);
		}
	}

}
