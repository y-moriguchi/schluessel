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
package net.morilib.lisp.net.http;

import java.net.HttpURLConnection;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/23
 */
public class HttpHeaders extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		Map<String,List<String>> mp;
		HttpURLConnection con;
		ConsListBuilder b, c;

		if(c1a instanceof LispHttpConnection) {
			con = ((LispHttpConnection)c1a).connection;
			if((mp = con.getHeaderFields()) == null) {
				return Nil.NIL;
			}

			b   = new ConsListBuilder();
			for(Map.Entry<String, List<String>> e1 : mp.entrySet()) {
				c = new ConsListBuilder();
				for(String e2 : e1.getValue()) {
					c.append(LispUtils.toLispString(e2));
				}
				b.append(new Cons(
						LispUtils.toLispString(e1.getKey()), c.get()));
			}
			return b.get();
		} else {
			throw mesg.getError("err.net.require.httpconnection", c1a);
		}
	}

}
