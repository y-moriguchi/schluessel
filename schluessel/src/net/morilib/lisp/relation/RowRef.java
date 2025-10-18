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
package net.morilib.lisp.relation;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/09
 */
public class RowRef extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispDBRow)) {
			throw mesg.getError("err.relation.require.resultrow", c1a);
		} else if(!(c2a instanceof Symbol)) {
			throw mesg.getError("err.require.symbol", c2a);
		}

		try {
			Datum d;

			d = ((LispDBRow)c1a).get(((Symbol)c2a).getName());
			return (d == null) ? Nil.NIL : d;
		} catch (LispRelationException e) {
			throw e.getLispException(mesg);
		}
	}

}
