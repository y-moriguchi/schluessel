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
package net.morilib.lisp.r6rs.io;

import java.io.ByteArrayOutputStream;
import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.OutputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.StringInputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.Transcoders;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.uvector.LispU8Vector;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class StringToBytevector extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		ByteArrayOutputStream ous;
		StringInputTranscoder tri;
		OutputTranscoder tro;
		String s = SubrUtils.getString(c1a, mesg);

		if(!(c2a instanceof ILispTranscoder)) {
			throw mesg.getError("err.io.require.transcoder", c2a);
		} else {
			try {
				ous = new ByteArrayOutputStream();
				tri = new StringInputTranscoder(s);
				tro = ((ILispTranscoder)c2a).newOutput(ous);
				Transcoders.copyWithClose(tri, tro);
				return new LispU8Vector(ous.toByteArray());
			} catch(IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			}
		}
	}

}
