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
package net.morilib.lisp.math.geometry.g3d;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/16
 */
public class F64Vector3D extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		double x = SubrUtils.getDouble(c1a, mesg);
		double y = SubrUtils.getDouble(c2a, mesg);
		double z = SubrUtils.getDouble(c3a, mesg);

		if(Double.isInfinite(x) || Double.isNaN(x)) {
			throw mesg.getError("err.math.require.notextreme", c1a);
		} else if(Double.isInfinite(y) || Double.isNaN(y)) {
			throw mesg.getError("err.math.require.notextreme", c2a);
		} else if(Double.isInfinite(z) || Double.isNaN(z)) {
			throw mesg.getError("err.math.require.notextreme", c3a);
		} else {
			return new LispF64Vector3D(x, y, z);
		}
	}

}
