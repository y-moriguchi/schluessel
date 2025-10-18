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
import net.morilib.lisp.r6rs.io.BytevectorToString;
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
public class BytevectorToStringTest extends TCSubr {

	public void testEval1() {
		LispU8Vector v = new LispU8Vector("田井中律".getBytes());
		BytevectorToString e = new BytevectorToString();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispUTF8Codec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.REPLACE);

		eq(e.eval(list(v, tr0), env, mesg), str("田井中律"));
	}

	public void testEval2() {
		LispU8Vector v = new LispU8Vector(new byte[] {
				(byte)0xb1, (byte)0xb2, (byte)0xc4, (byte)0xd9, (byte)0xe1,
				(byte)0xcf, (byte)0xbd, (byte)0xc0, (byte)0xb0, (byte)0xa0,
		});
		BytevectorToString e = new BytevectorToString();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispTstCodec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.REPLACE);

		eq(e.eval(list(v, tr0), env, mesg), str("アイトル?マスター?"));
	}

	public void testEval3() {
		LispU8Vector v = new LispU8Vector(new byte[] {
				(byte)0xb1, (byte)0xb2, (byte)0xc4, (byte)0xd9, (byte)0xe1,
				(byte)0xcf, (byte)0xbd, (byte)0xc0, (byte)0xb0, (byte)0xa0,
		});
		BytevectorToString e = new BytevectorToString();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispTstCodec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.IGNORE);

		eq(e.eval(list(v, tr0), env, mesg), str("アイトルマスター"));
	}

	public void testEval4() {
		LispU8Vector v = new LispU8Vector(new byte[] {
				(byte)0xb1, (byte)0xb2, (byte)0xc4, (byte)0xd9, (byte)0xe1,
				(byte)0xcf, (byte)0xbd, (byte)0xc0, (byte)0xb0, (byte)0xa0,
		});
		BytevectorToString e = new BytevectorToString();
		Environment env = new Environment();
		LispMessage mesg = LispMessage.getInstance();
		LispTranscoder tr0 =
			new LispTranscoder(LispTstCodec.INSTANCE,
					LispEolStyle.LF,
					LispErrorHandlingMode.RAISE);

		try {
			e.eval(list(v, tr0), env, mesg);  fail();
		} catch(LispException z) {}
	}

}
