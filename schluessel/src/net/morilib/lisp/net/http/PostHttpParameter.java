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

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.net.HttpURLConnection;
import java.util.Map;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/08/17
 */
public class PostHttpParameter extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		Datum c3a = Iterators.nextIf(itr);
		Map<Datum, Datum> prm;
		HttpURLConnection con;
		PrintWriter prt = null;
		String dlm = "";

		try {
			SubrUtils.checkTerminated(itr, body, mesg);
			if(!(c1a instanceof LispHttpConnection)) {
				throw mesg.getError("err.net.require.httpconnection",
						c1a);
			} else if(!"POST".equals(
					(con = ((LispHttpConnection)c1a).connection)
					.getRequestMethod().toUpperCase())) {
				throw mesg.getError("err.net.httpmethod.invalid");
			} else if(c3a == null) {
				if((prm = LispUtils.assocToMap(c2a)) == null) {
					throw mesg.getError("err.require.assoc", c2a);
				}
				prt = new PrintWriter(new OutputStreamWriter(
						con.getOutputStream()));
			} else {
				if((prm = LispUtils.assocToMap(c3a)) == null) {
					throw mesg.getError("err.require.assoc", c3a);
				}
				prt = new PrintWriter(new OutputStreamWriter(
						con.getOutputStream(),
						SubrUtils.getString(c2a, mesg)));
			}

			for(Map.Entry<Datum, Datum> e : prm.entrySet()) {
				if(!(e.getKey() instanceof LispString)) {
					throw mesg.getError("err.require.string",
							e.getKey());
				}
				prt.print(dlm);
				prt.print(e.getKey().getString());
				prt.print("=");
				prt.print(LispUtils.print(e.getValue()));
				dlm = "&";
			}
		} catch(IOException e) {
			throw mesg.getError("err.io");
		} finally {
			if(prt != null) {
				prt.close();
			}
		}
		return Undef.UNDEF;
	}

}
