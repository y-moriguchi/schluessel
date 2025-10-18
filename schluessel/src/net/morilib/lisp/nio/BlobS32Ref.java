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
package net.morilib.lisp.nio;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.SRFI74Endianness;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/11
 */
public class BlobS32Ref extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		if(!(c1a instanceof SRFI74Endianness.Endian)) {
			throw mesg.getError("err.srfi74.require.endianness", c1a);
		} else if(c2a instanceof LispBlob) {
			int k = SubrUtils.getSmallInt(c3a, mesg);

			try {
				((LispBlob)c1a).order((SRFI74Endianness.Endian)c2a);
				return LispInteger.valueOf(
						((LispBlob)c2a).getInt(k));
			} catch(IndexOutOfBoundsException e) {
				throw mesg.getError("err.srfi74.blob.outofrange",
						"" + k);
			}
		} else {
			throw mesg.getError("err.srfi74.require.blob", c2a);
		}
	}

}
