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
package net.morilib.lisp.regex;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.RegexPattern;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
public class Rxmatch extends BinaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(
			Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof RegexPattern) {
			String  s = SubrUtils.getString(c2a, mesg);
			Pattern p = ((RegexPattern)c1a).getPattern();
			List<Datum> b = new ArrayList<Datum>();
			Matcher m = p.matcher(s);

			if(!m.matches()) {
				return LispBoolean.FALSE;
			} else {
				for(int i = 0; i <= m.groupCount(); i++) {
					b.add(new LispString(m.group(i)));
				}
			}
			return MultiValues.newValues(b);
		} else {
			throw mesg.getError("err.regexp.require.regexp", c1a);
		}
	}

}
