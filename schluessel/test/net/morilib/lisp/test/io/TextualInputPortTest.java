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
import net.morilib.lisp.EOFObject;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.r6rs.io.GetChar;
import net.morilib.lisp.r6rs.io.GetDatum;
import net.morilib.lisp.r6rs.io.GetLine;
import net.morilib.lisp.r6rs.io.GetStringAll;
import net.morilib.lisp.r6rs.io.GetStringN;
import net.morilib.lisp.r6rs.io.GetStringNS;
import net.morilib.lisp.r6rs.io.LookaheadChar;
import net.morilib.lisp.r6rs.io.OpenStringInputPort;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class TextualInputPortTest extends TCSubr {

	public void testEval1() {
		LispString v  = str("双海 亜美・真美 十勝");
		LispString v2 = str("     ");
		OpenStringInputPort e1 = new OpenStringInputPort();
		GetChar e2 = new GetChar();
		LookaheadChar e3 = new LookaheadChar();
		GetStringN e4 = new GetStringN();
		GetStringNS e5 = new GetStringNS();
		GetStringAll e6 = new GetStringAll();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		Datum x1 = e1.eval(list(v), env, mesg);

		eq(e3.eval(list(x1), env, mesg), chr('双'));
		eq(e2.eval(list(x1), env, mesg), chr('双'));
		eq(e3.eval(list(x1), env, mesg), chr('海'));
		eq(e2.eval(list(x1), env, mesg), chr('海'));
		eq(e4.eval(list(x1, newZ(3)), env, mesg), str(" 亜美"));
		eq(e3.eval(list(x1), env, mesg), chr('・'));
		e5.eval(list(x1, v2, newZ(1), newZ(2)), env, mesg);
		eq(v2, str(" ・真  "));
		eq(e3.eval(list(x1), env, mesg), chr('美'));
		eq(e6.eval(list(x1), env, mesg), str("美 十勝"));
		eq(e3.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e4.eval(list(x1, newZ(1)), env, mesg), EOFObject.EOF);
		eq(e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg),
				EOFObject.EOF);
		eq(e6.eval(list(x1), env, mesg), EOFObject.EOF);
	}

	public void testEval2() {
		LispString v  = str("line\n(1 2 3)a(1)bbb(1)cc(1)dd");
		LispString v2 = str("     ");
		OpenStringInputPort e1 = new OpenStringInputPort();
		GetChar e2 = new GetChar();
		LookaheadChar e3 = new LookaheadChar();
		GetStringN e4 = new GetStringN();
		GetStringNS e5 = new GetStringNS();
		GetStringAll e6 = new GetStringAll();
		GetLine e7 = new GetLine();
		GetDatum e8 = new GetDatum();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		Datum x1 = e1.eval(list(v), env, mesg);

		eq(e7.eval(list(x1), env, mesg), str("line"));
		equal(e8.eval(list(x1), env, mesg), list(1, 2, 3));
		eq(e3.eval(list(x1), env, mesg), chr('a'));
		eq(e2.eval(list(x1), env, mesg), chr('a'));
		equal(e8.eval(list(x1), env, mesg), list(1));
		eq(e3.eval(list(x1), env, mesg), chr('b'));
		eq(e4.eval(list(x1, 3), env, mesg), str("bbb"));
		equal(e8.eval(list(x1), env, mesg), list(1));
		eq(e3.eval(list(x1), env, mesg), chr('c'));
		e5.eval(list(x1, v2, newZ(1), newZ(2)), env, mesg);
		eq(v2, str(" cc  "));
		equal(e8.eval(list(x1), env, mesg), list(1));
		eq(e3.eval(list(x1), env, mesg), chr('d'));
		eq(e6.eval(list(x1), env, mesg), str("dd"));
		eq(e3.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e4.eval(list(x1, newZ(1)), env, mesg), EOFObject.EOF);
		eq(e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg),
				EOFObject.EOF);
		eq(e6.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e7.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e8.eval(list(x1), env, mesg), EOFObject.EOF);
	}

}
