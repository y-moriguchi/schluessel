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
import net.morilib.lisp.r6rs.io.GetStringAll;
import net.morilib.lisp.r6rs.io.OpenBytevectorInputPort;
import net.morilib.lisp.r6rs.io.TranscodedPort;
import net.morilib.lisp.r6rs.io.transcd.LispEolStyle;
import net.morilib.lisp.r6rs.io.transcd.LispErrorHandlingMode;
import net.morilib.lisp.r6rs.io.transcd.LispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispUTF8Codec;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class TranscodedPortTest extends TCSubr {

	public void testEval1() {
		LispU8Vector v = new LispU8Vector("田井中律".getBytes());
		OpenBytevectorInputPort e1 = new OpenBytevectorInputPort();
		TranscodedPort e2 = new TranscodedPort();
		GetStringAll e3 = new GetStringAll();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispUTF8Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.REPLACE);
		Datum x1 = e1.eval(list(v), env, mesg);
		Datum x2 = e2.eval(list(x1, tr0), env, mesg);

		eq(e3.eval(list(x2), env, mesg), str("田井中律"));
	}

}
