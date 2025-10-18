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

import java.io.ByteArrayOutputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.r6rs.io.LispOutputStreamPort;
import net.morilib.lisp.r6rs.io.OpenBytevectorOutputPort;
import net.morilib.lisp.r6rs.io.PutBytevector;
import net.morilib.lisp.r6rs.io.PutU8;
import net.morilib.lisp.r6rs.io.transcd.LispBufferMode;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class BinaryOutputPortTest extends TCSubr {

	public void testEval1() {
		LispU8Vector v  = new LispU8Vector("MiuraAzusa".getBytes());
		OpenBytevectorOutputPort e1 = new OpenBytevectorOutputPort();
		PutU8 e2 = new PutU8();
		PutBytevector e3 = new PutBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		MultiValues x1 = (MultiValues)e1.eval(list(), env, mesg);
		Datum x2 = x1.getValues().get(0);
		Subr e4 = (Subr)x1.getValues().get(1);

		e2.eval(list(x2, newZ('@')), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "@".getBytes());
		e3.eval(list(x2, v), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "@MiuraAzusa".getBytes());
		e3.eval(list(x2, v, 5), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "@MiuraAzusaAzusa".getBytes());
		e3.eval(list(x2, v, 5, 3), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "@MiuraAzusaAzusaAzu".getBytes());
	}

	public void testEval2() {
		LispU8Vector v  = new LispU8Vector("MiuraAzusa".getBytes());
		PutU8 e2 = new PutU8();
		PutBytevector e3 = new PutBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		ByteArrayOutputStream ous = new ByteArrayOutputStream();
		LispOutputStreamPort x2 = new LispOutputStreamPort(ous,
				LispBufferMode.NONE);

		e2.eval(list(x2, newZ('@')), env, mesg);
		eq(ous.toByteArray(), "@".getBytes());
		e3.eval(list(x2, v), env, mesg);
		eq(ous.toByteArray(), "@MiuraAzusa".getBytes());
		e3.eval(list(x2, v, 5), env, mesg);
		eq(ous.toByteArray(), "@MiuraAzusaAzusa".getBytes());
		e3.eval(list(x2, v, 5, 3), env, mesg);
		eq(ous.toByteArray(), "@MiuraAzusaAzusaAzu".getBytes());
	}

}
