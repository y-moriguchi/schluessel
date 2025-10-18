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
package net.morilib.lisp.iterator;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/29
 */
public class IteratorToList extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		ILispIterator itr;
		ConsListBuilder b = new ConsListBuilder();

		if(c1a instanceof ILispIterator) {
			itr = (ILispIterator)c1a;
		} else if(c1a instanceof ILispIterable) {
			itr = ((ILispIterable)c1a).lispIterator();
		} else {
			throw mesg.getError("err.iterator.require.iterator", c1a);
		}

		for(; !itr.isTerminated(); itr = itr.next()) {
			b.append(itr.getCurrentDatum());
		}
		return b.get();
	}

}
