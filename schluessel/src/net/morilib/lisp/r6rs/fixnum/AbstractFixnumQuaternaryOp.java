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
package net.morilib.lisp.r6rs.fixnum;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/29
 */
public abstract class AbstractFixnumQuaternaryOp
extends QuaternaryArgs {

	/**
	 * 
	 * @param x
	 * @param y
	 * @param z
	 * @param env
	 * @param mesg
	 * @return
	 */
	protected abstract int op1(int x, int y, int z, int w,
			LispMessage mesg);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		int x = SubrUtils.getSmallInt(c1a, mesg);
		int y = SubrUtils.getSmallInt(c2a, mesg);
		int z = SubrUtils.getSmallInt(c3a, mesg);
		int w = SubrUtils.getSmallInt(c4a, mesg);

		return LispInteger.valueOf(op1(x, y, z, w, mesg));
	}

}
