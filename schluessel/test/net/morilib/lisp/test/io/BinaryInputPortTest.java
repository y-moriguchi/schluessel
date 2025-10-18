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

import java.io.ByteArrayInputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.EOFObject;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.r6rs.io.GetBytevectorAll;
import net.morilib.lisp.r6rs.io.GetBytevectorN;
import net.morilib.lisp.r6rs.io.GetBytevectorNS;
import net.morilib.lisp.r6rs.io.GetU8;
import net.morilib.lisp.r6rs.io.LispInputStreamPort;
import net.morilib.lisp.r6rs.io.LookaheadU8;
import net.morilib.lisp.r6rs.io.OpenBytevectorInputPort;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.ILispBytevector;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class BinaryInputPortTest extends TCSubr {

	public void testEval1() {
		LispU8Vector v  = new LispU8Vector("MiuraAzusa".getBytes());
		LispU8Vector v2 = new LispU8Vector("     ".getBytes());
		OpenBytevectorInputPort e1 = new OpenBytevectorInputPort();
		GetU8 e2 = new GetU8();
		LookaheadU8 e3 = new LookaheadU8();
		GetBytevectorN e4 = new GetBytevectorN();
		GetBytevectorNS e5 = new GetBytevectorNS();
		GetBytevectorAll e6 = new GetBytevectorAll();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		Datum x1 = e1.eval(list(v), env, mesg);

		eq(e3.eval(list(x1), env, mesg), newZ('M'));
		eq(e2.eval(list(x1), env, mesg), newZ('M'));
		eq(e3.eval(list(x1), env, mesg), newZ('i'));
		eq(e2.eval(list(x1), env, mesg), newZ('i'));
		eq(((ILispBytevector)e4.eval(list(x1, newZ(3)), env, mesg)).toBytes(),
				"ura".getBytes());
		eq(e3.eval(list(x1), env, mesg), newZ('A'));
		e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg);
		eq(v2.toBytes(), " Azu ".getBytes());
		eq(e3.eval(list(x1), env, mesg), newZ('s'));
		eq(((ILispBytevector)e6.eval(list(x1), env, mesg)).toBytes(),
				"sa".getBytes());
		eq(e3.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e4.eval(list(x1, newZ(1)), env, mesg), EOFObject.EOF);
		eq(e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg),
				EOFObject.EOF);
		eq(e6.eval(list(x1), env, mesg), EOFObject.EOF);
	}

	public void testEval2() {
		LispU8Vector v2 = new LispU8Vector("     ".getBytes());
		GetU8 e2 = new GetU8();
		LookaheadU8 e3 = new LookaheadU8();
		GetBytevectorN e4 = new GetBytevectorN();
		GetBytevectorNS e5 = new GetBytevectorNS();
		GetBytevectorAll e6 = new GetBytevectorAll();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispInputStreamPort x1 = new LispInputStreamPort(
				new ByteArrayInputStream("MiuraAzusa".getBytes()));

		eq(e3.eval(list(x1), env, mesg), newZ('M'));
		eq(e2.eval(list(x1), env, mesg), newZ('M'));
		eq(e3.eval(list(x1), env, mesg), newZ('i'));
		eq(e2.eval(list(x1), env, mesg), newZ('i'));
		eq(((ILispBytevector)e4.eval(list(x1, newZ(3)), env, mesg)).toBytes(),
				"ura".getBytes());
		eq(e3.eval(list(x1), env, mesg), newZ('A'));
		e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg);
		eq(v2.toBytes(), " Azu ".getBytes());
		eq(e3.eval(list(x1), env, mesg), newZ('s'));
		eq(((ILispBytevector)e6.eval(list(x1), env, mesg)).toBytes(),
				"sa".getBytes());
		eq(e3.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e4.eval(list(x1, newZ(1)), env, mesg), EOFObject.EOF);
		eq(e5.eval(list(x1, v2, newZ(1), newZ(3)), env, mesg),
				EOFObject.EOF);
		eq(e6.eval(list(x1), env, mesg), EOFObject.EOF);
	}

}
