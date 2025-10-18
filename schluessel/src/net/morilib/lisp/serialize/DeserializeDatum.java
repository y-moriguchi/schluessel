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

import java.io.EOFException;
import java.io.IOException;
import java.io.ObjectInputStream;

import net.morilib.lisp.Datum;
import net.morilib.lisp.EOFObject;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispJavaUtils;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.r6rs.io.ILispBinaryInputPort;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/12
 */
public class DeserializeDatum extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		ObjectInputStream ois;

		if(c1a instanceof ILispBinaryInputPort) {
			try {
				ois = new ObjectInputStream(
						((ILispBinaryInputPort)c1a).getInputStream());
				return LispJavaUtils.newInstance(ois.readObject());
			} catch (EOFException e) {
				return EOFObject.EOF;
			} catch (IOException e) {
				throw mesg.getError("err.io", e.getMessage());
			} catch (ClassNotFoundException e) {
				throw mesg.getError("err.serialize.cannot.serialize");
			}
		} else {
			throw mesg.getError("err.io.require.port.textual.input",
					c1a);
		}
	}

}
