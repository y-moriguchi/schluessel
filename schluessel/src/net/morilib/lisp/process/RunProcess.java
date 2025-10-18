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
package net.morilib.lisp.process;

import java.io.IOException;
import java.util.Map;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/21
 */
public class RunProcess extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = Iterators.nextIf(itr);
		Map<Symbol, Datum> envp = null;
		String dir = null;

		if(c2a instanceof LispString) {
			dir = c2a.getString();
		} else if(c2a != null) {
			envp = LispUtils.assocToMapSymbol(c2a);
			dir = SubrUtils.nextString(itr, null, mesg);
		}

		SubrUtils.checkTerminated(itr, body, mesg);
		try {
			return LispInteger.valueOf(ProcessSubrUtils.runProcess(
					new ConsIterator(c1a), envp, dir, null));
		} catch(ProcessSyntaxException e) {
			throw mesg.getError("err.process.processsyntax", c1a);
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
