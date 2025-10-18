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
package net.morilib.lisp.serialize;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.file.LispFiles;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Bytes;
import net.morilib.util.IOs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/13
 */
public class GuessSerializeFile extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		File fn;
		InputStream ins = null;
		byte[] buf = new byte[2];

		try {
			fn  = LispFiles.getFile(env, c1a, mesg);
			ins = new FileInputStream(fn);
			if(ins.read(buf) < 2 ||
					buf[0] != (byte)0xac ||
					buf[1] != (byte)0xed) {
				return LispBoolean.FALSE;
			} else if(ins.read(buf) < 2) {
				return LispBoolean.FALSE;
			} else {
				return LispInteger.valueOf(
						Bytes.ubyteToInt(buf[0]) << 8 |
						Bytes.ubyteToInt(buf[1]));
			}
		} catch (IOException e) {
			throw mesg.getError("err.io");
		} finally {
			IOs.close(ins);
		}
	}

}
