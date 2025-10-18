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
package net.morilib.lisp.test.io;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.r6rs.io.OpenStringOutputPort;
import net.morilib.lisp.r6rs.io.PutChar;
import net.morilib.lisp.r6rs.io.PutDatum;
import net.morilib.lisp.r6rs.io.PutString;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class TextualOutputPortTest extends TCSubr {

	public void testEval1() {
		LispString v  = str("如月千早");
		OpenStringOutputPort e1 = new OpenStringOutputPort();
		PutChar e2 = new PutChar();
		PutString e3 = new PutString();
		PutDatum e5 = new PutDatum();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		MultiValues x1 = (MultiValues)e1.eval(list(), env, mesg);
		Datum x2 = x1.getValues().get(0);
		Subr e4 = (Subr)x1.getValues().get(1);

		e2.eval(list(x2, '@'), env, mesg);
		eq(e4.eval(list(), env, mesg), str("@"));
		e3.eval(list(x2, v), env, mesg);
		eq(e4.eval(list(), env, mesg), str("@如月千早"));
		e3.eval(list(x2, v, 2), env, mesg);
		eq(e4.eval(list(), env, mesg), str("@如月千早千早"));
		e3.eval(list(x2, v, 1, 2), env, mesg);
		eq(e4.eval(list(), env, mesg), str("@如月千早千早月千"));
		e5.eval(list(x2, list(72, 55, 78)), env, mesg);
		eq(e4.eval(list(), env, mesg),
				str("@如月千早千早月千(72 55 78)"));
	}

}
