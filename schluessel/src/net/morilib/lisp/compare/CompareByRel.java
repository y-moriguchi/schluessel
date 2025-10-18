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
package net.morilib.lisp.compare;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/07/24
 */
public abstract class CompareByRel extends Subr {

	//
	private static class Proc extends BinaryArgs {

		//
		private Procedure proc;
		private CompareByRel rel;

		//
		private Proc(Procedure p, CompareByRel rel) {
			this.proc = p;
			this.rel  = rel;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum x, Datum y, Environment env,
				LispMessage mesg) {
			boolean cxy, cyx;

			cxy = Scheme.callva(proc, env, mesg, x, y).isTrue();
			cyx = Scheme.callva(proc, env, mesg, y, x).isTrue();
			return LispInteger.valueOf(rel.rel(cxy, cyx));
		}

	}

	//
	/*package*/ abstract int rel(boolean cxy, boolean cyx);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum p = SubrUtils.nextIf(itr, mesg, body);
		Datum x = Iterators.nextIf(itr), y;
		boolean cxy, cyx;

		if(x == null) {
			return new Proc((Procedure)p, this);
		} else {
			y = SubrUtils.nextIf(itr, mesg, body);
			SubrUtils.checkTerminated(itr, body, mesg);
			cxy = Scheme.callva(p, env, mesg, x, y).isTrue();
			cyx = Scheme.callva(p, env, mesg, y, x).isTrue();
			return LispInteger.valueOf(rel(cxy, cyx));
		}
	}

}
