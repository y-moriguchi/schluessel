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

import net.morilib.lisp.Environment;
import net.morilib.lisp.LispException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.r6rs.io.StringToBytevector;
import net.morilib.lisp.r6rs.io.transcd.LispEolStyle;
import net.morilib.lisp.r6rs.io.transcd.LispErrorHandlingMode;
import net.morilib.lisp.r6rs.io.transcd.LispLatin1Codec;
import net.morilib.lisp.r6rs.io.transcd.LispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.LispUTF8Codec;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.uvector.ILispBytevector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/09
 */
public class StringToBytevectorTest extends TCSubr {

	public void testEval1() {
		StringToBytevector e = new StringToBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispUTF8Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.REPLACE);
		ILispBytevector v =
			(ILispBytevector)e.eval(list(str("秋山澪"), tr0), env, mesg);

		eq(v.toBytes(), "秋山澪".getBytes());
	}

	public void testEval2() {
		StringToBytevector e = new StringToBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispLatin1Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.REPLACE);
		ILispBytevector v =
			(ILispBytevector)e.eval(list(str("Mio〜"), tr0), env, mesg);

		eq(v.toBytes(), "Mio?".getBytes());
	}

	public void testEval3() {
		StringToBytevector e = new StringToBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispLatin1Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.IGNORE);
		ILispBytevector v =
			(ILispBytevector)e.eval(list(str("Mio〜"), tr0), env, mesg);

		eq(v.toBytes(), "Mio".getBytes());
	}

	public void testEval4() {
		StringToBytevector e = new StringToBytevector();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispLatin1Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.RAISE);

		try {
			e.eval(list(str("Mio〜"), tr0), env, mesg); fail();
		} catch(LispException z) {}
	}

}
