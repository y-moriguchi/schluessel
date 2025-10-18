/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp.subr;

import java.util.ArrayList;
import java.util.List;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class StringToList extends StringBase {

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.subr.StringBase#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	protected Datum execute(
			Datum c1a, Environment env, LispMessage mesg) {
		if(!(c1a instanceof LispString)) {
			throw mesg.getError("err.require.string", c1a);
		}
		
		List<Datum> res = new ArrayList<Datum>();
		String str = ((LispString)c1a).getString();
		
		for(int i = 0; i < str.length(); i++) {
			res.add(new LispCharacter(str.charAt(i)));
		}
		
		return LispUtils.listToCons(res);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.StringBase#execute(java.lang.String, int, int)
	 */
	@Override
	protected Datum execute(String s, int b, int e, LispMessage mesg) {
		ConsListBuilder res = new ConsListBuilder();

		for(int i = b; i < e; i++) {
			res.append(new LispCharacter(s.charAt(i)));
		}
		return res.get();
	}

}
