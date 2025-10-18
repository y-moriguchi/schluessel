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
package net.morilib.lisp.exlib;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/31
 */
public class DefineReaderCtor extends BinaryArgs {

	//
	private static Map<Datum, Procedure> ctormap =
		new ConcurrentHashMap<Datum, Procedure>();

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/31
	 */
	public static class LookupReaderCtor extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			Datum d = (Datum)ctormap.get(c1a);

			if(d == null) {
				throw mesg.getError("err.srfi10.require.undefined",
						c1a);
			}
			return d;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof Symbol)) {
			throw mesg.getError("err.require.symbol", c2a);
		} else if(!(c2a instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", c2a);
		} else {
			ctormap.put(c1a, (Procedure)c2a);
			return Undef.UNDEF;
		}
	}

}
