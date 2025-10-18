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
package net.morilib.lisp.math.geometry.g2d;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SenaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/06/21
 */
public class MakeAffineTransform2d extends SenaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.SenaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Datum c5a, Datum c6a, Environment env, LispMessage mesg) {
		double scaleX = SubrUtils.getDouble(c1a, mesg);
		double shearX = SubrUtils.getDouble(c2a, mesg);
		double shearY = SubrUtils.getDouble(c3a, mesg);
		double scaleY = SubrUtils.getDouble(c4a, mesg);
		double transX = SubrUtils.getDouble(c5a, mesg);
		double transY = SubrUtils.getDouble(c6a, mesg);

		return new LispDoubleAffineTransform2D(
				scaleX, scaleY, shearX, shearY, transX, transY);
	}

}
