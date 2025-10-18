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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/01/01
 */
public class StringToUuid extends UnaryArgs {

	//
	private static final String PTNS =
		"([0-9A-Fa-f]{8})-([0-9A-Fa-f]{4})-([0-9A-Fa-f]{4})-" +
		"([0-9A-Fa-f]{4})-([0-9A-Fa-f]{12})";
	private static final Pattern PTN = Pattern.compile(PTNS);

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		String  s = SubrUtils.getString(c1a, mesg), c;
		Matcher m = PTN.matcher(s);
		BigInteger b;

		if(m.matches()) {
			c = (m.group(1) + m.group(2) + m.group(3) + m.group(4) +
					m.group(5));
			b = new BigInteger(c, 16);
			return new LispUUID(new UUID(
					b.shiftRight(64).longValue(), b.longValue()));
		} else {
			throw mesg.getError("err.uuid.format.invalid", c1a);
		}
	}

}
