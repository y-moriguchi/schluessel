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
package net.morilib.lisp.uuid;

import java.math.BigInteger;
import java.util.UUID;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/30
 */
public class MakeUuid extends UnaryArgs {

	//
	private static final BigInteger MAX =
		new BigInteger("100000000000000000000000000000000", 16);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		BigInteger id = SubrUtils.getBigInteger(c1a, mesg);
		long m, l;

		id = id.remainder(MAX);
		m  = id.shiftRight(64).longValue();
		l  = id.longValue();
		return new LispUUID(new UUID(m, l));
	}

}
