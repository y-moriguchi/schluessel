/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.ILispVector;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispVector;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class VectorToList extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(c1a instanceof LispVector) {
			return ((LispVector)c1a).toConsList();
		} else if(c1a instanceof ILispVector) {
			ConsListBuilder b = new ConsListBuilder();
			ILispVector v = (ILispVector)c1a;

			for(int i = 0; i < v.size(); i++) {
				b.append(v.get(i));
			}
			return b.get();
		} else {
			//throw new LispException("vector required");
			throw mesg.getError("err.require.vector", c1a);
		}
	}

}
