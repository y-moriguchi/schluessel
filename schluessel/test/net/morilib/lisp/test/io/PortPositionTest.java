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
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Subr;
import net.morilib.lisp.r6rs.io.GetBytevectorN;
import net.morilib.lisp.r6rs.io.LookaheadU8;
import net.morilib.lisp.r6rs.io.OpenBytevectorInputPort;
import net.morilib.lisp.r6rs.io.OpenBytevectorOutputPort;
import net.morilib.lisp.r6rs.io.PortPosition;
import net.morilib.lisp.r6rs.io.PutBytevector;
import net.morilib.lisp.r6rs.io.SetPortPositionS;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class PortPositionTest extends TCSubr {

	public void testEval1() {
		LispU8Vector v  = new LispU8Vector("MiuraAzusa".getBytes());
		OpenBytevectorInputPort e1 = new OpenBytevectorInputPort();
		LookaheadU8 e3 = new LookaheadU8();
		GetBytevectorN e4 = new GetBytevectorN();
		PortPosition e5 = new PortPosition();
		SetPortPositionS e6 = new SetPortPositionS();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		Datum x1 = e1.eval(list(v), env, mesg);

		eq(e5.eval(list(x1), env, mesg), newZ(0));
		e6.eval(list(x1, 5), env, mesg);
		eq(e3.eval(list(x1), env, mesg), newZ('A'));
		eqbv(e4.eval(list(x1, newZ(3)), env, mesg), "Azu".getBytes());
		e6.eval(list(x1, 10), env, mesg);
		eq(e3.eval(list(x1), env, mesg), EOFObject.EOF);
		eq(e4.eval(list(x1, newZ(1)), env, mesg), EOFObject.EOF);
		e6.eval(list(x1, 5), env, mesg);
		eq(e3.eval(list(x1), env, mesg), newZ('A'));
		eqbv(e4.eval(list(x1, newZ(3)), env, mesg), "Azu".getBytes());
	}

	public void testEval2() {
		LispU8Vector v  = new LispU8Vector("MiuraAzusa".getBytes());
		OpenBytevectorOutputPort e1 = new OpenBytevectorOutputPort();
		PutBytevector e3 = new PutBytevector();
		PortPosition e5 = new PortPosition();
		SetPortPositionS e6 = new SetPortPositionS();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		MultiValues x1 = (MultiValues)e1.eval(list(), env, mesg);
		Datum x2 = x1.getValues().get(0);
		Subr e4 = (Subr)x1.getValues().get(1);

		e3.eval(list(x2, v), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "MiuraAzusa".getBytes());
		eq(e5.eval(list(x2), env, mesg), newZ(10));
		e6.eval(list(x2, 5), env, mesg);
		e3.eval(list(x2, v), env, mesg);
		eqbv(e4.eval(list(), env, mesg), "MiuraMiuraAzusaAzusa".getBytes());
	}

}
