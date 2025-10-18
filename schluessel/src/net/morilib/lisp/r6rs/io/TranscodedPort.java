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

import java.io.IOException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.r6rs.io.transcd.ILispTranscoder;
import net.morilib.lisp.r6rs.io.transcd.InputTranscoder;
import net.morilib.lisp.r6rs.io.transcd.OutputTranscoder;
import net.morilib.lisp.subr.BinaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/03
 */
public class TranscodedPort extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		try {
			if(!(c2a instanceof ILispTranscoder)) {
				throw mesg.getError("err.io.require.transcoder", c2a);
			} else if(c1a instanceof ILispBinaryInputPort) {
				InputTranscoder tr = ((ILispTranscoder)c2a).newInput(
						((ILispBinaryInputPort)c1a).getInputStream());

				return new LispTextualInputPort(
						tr, (ILispTranscoder)c2a);
			} else if(c1a instanceof ILispBinaryOutputPort) {
				OutputTranscoder tr = ((ILispTranscoder)c2a).newOutput(
						((ILispBinaryOutputPort)c1a).getOutputStream());

				return new LispTextualOutputPort(
						tr, (ILispTranscoder)c2a,
						((ILispBinaryOutputPort)c1a).getBufferMode());
			} else {
				throw mesg.getError("err.io.require.port.binary", c1a);
			}
		} catch(IOException e) {
			throw mesg.getError("err.io", e.getMessage());
		}
	}

}
