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
package net.morilib.lisp.net.uri;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/22
 */
public class MakeUri extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		String scheme, ssp, fragment, userInfo, host, path, query;
		String authority, uri;
		int port;

		try {
			switch(l.size()) {
			case 1:
				uri       = SubrUtils.getString(l.get(0), mesg);
				return new LispURI(new URI(uri));
			case 3:
				scheme    = SubrUtils.getString(l.get(0), mesg);
				ssp       = SubrUtils.getString(l.get(1), mesg);
				fragment  = SubrUtils.getString(l.get(2), mesg);
				return new LispURI(new URI(scheme, ssp, fragment));
			case 4:
				scheme    = SubrUtils.getString(l.get(0), mesg);
				host      = SubrUtils.getString(l.get(1), mesg);
				path      = SubrUtils.getString(l.get(2), mesg);
				fragment  = SubrUtils.getString(l.get(3), mesg);
				return new LispURI(new URI(
						scheme, host, path, fragment));
			case 5:
				scheme    = SubrUtils.getString(l.get(0), mesg);
				authority = SubrUtils.getString(l.get(1), mesg);
				path      = SubrUtils.getString(l.get(2), mesg);
				query     = SubrUtils.getString(l.get(3), mesg);
				fragment  = SubrUtils.getString(l.get(4), mesg);
				return new LispURI(new URI(
						scheme, authority, path, query, fragment));
			case 7:
				scheme    = SubrUtils.getString(l.get(0), mesg);
				userInfo  = SubrUtils.getString(l.get(3), mesg);
				host      = SubrUtils.getString(l.get(1), mesg);
				port      = SubrUtils.getSmallInt(l.get(3), mesg);
				path      = SubrUtils.getString(l.get(2), mesg);
				query     = SubrUtils.getString(l.get(3), mesg);
				fragment  = SubrUtils.getString(l.get(3), mesg);
				return new LispURI(new URI(
						scheme, userInfo, host, port, path,
						query, fragment));
			default:
				throw mesg.getError("err.argument", body);
			}
		} catch(URISyntaxException e) {
			throw mesg.getError("err.net.uri.invalid");
		}
	}

}
