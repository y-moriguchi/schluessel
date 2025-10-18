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

import java.io.IOException;
import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.InputPort;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/16
 */
public class AskList extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum d1 = SubrUtils.nextIf(itr, mesg, body);
		List<Datum> l;
		ILispLineReadable p;
		String prompt;
		int s;

		if(d1 instanceof ILispLineReadable) {
			l = LispUtils.consToList(
					SubrUtils.nextIf(itr, mesg, body), mesg);
			p = (ILispLineReadable)d1;
		} else {
			l = LispUtils.consToList(d1, mesg);
			p = InputPort.getStandard(mesg);
		}
		prompt = SubrUtils.nextString(itr, "#?", mesg);
		SubrUtils.checkTerminated(itr, body, mesg);

		for(int i = 0; i < l.size(); i++) {
			System.out.printf("%d) %s\n",
					i + 1, LispUtils.print(l.get(i)));
		}
		System.out.print(prompt);
		System.out.print(" ");

		try {
			s = Integer.parseInt(p.getLine());
			return (s > 0 && s <= l.size()) ?
					l.get(s - 1) : LispBoolean.FALSE;
		} catch(NumberFormatException e) {
			return LispBoolean.FALSE;
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
