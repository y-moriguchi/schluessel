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
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.QuinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/06/12
 */
public class BlobCopyS extends QuinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Datum c5a, Environment env, LispMessage mesg) {
		int ss = SubrUtils.getSmallInt(c2a, mesg);
		int ts = SubrUtils.getSmallInt(c4a, mesg);
		int n  = SubrUtils.getSmallInt(c5a, mesg);
		LispBlob bs, bt;

		if(!(c1a instanceof LispBlob)) {
			throw mesg.getError("err.srfi74.require.blob", c1a);
		} else if(!(c3a instanceof LispBlob)) {
			throw mesg.getError("err.srfi74.require.blob", c2a);
		}

		bs = ((LispBlob)c1a);
		bt = ((LispBlob)c3a);
		if(n < 0) {
			throw mesg.getError("err.require.int.nonnegative", c5a);
		} else if(!(ss >= 0 && bs.length() >= ss + n)) {
			throw mesg.getError("err.srfi74.index.invalid", c2a);
		} else {
			for(int i = 0; i < n; i++) {
				bt.put(ts + 1, bs.get(ss + i));
			}
			return Undef.UNDEF;
		}
	}

}
