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
package net.morilib.lisp.phys.unit;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.phys.unit.Quantity;
import net.morilib.phys.unit.Units;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/05
 */
public class MakeQuantity extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		double d = SubrUtils.getDouble(c1a, mesg);
		String u = SubrUtils.getString(c2a, mesg);
		Quantity q = Units.parse(LispUnitSystem.defaultSystem.sys, u);

		if(q == null) {
			throw mesg.getError("err.unit.unit.invalid", u);
		} else {
			return new LispQuantity(d * q.getValue(), q.getUnit());
		}
	}

}
