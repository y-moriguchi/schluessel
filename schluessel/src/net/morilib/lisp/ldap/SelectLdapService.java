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
package net.morilib.lisp.ldap;

import java.util.List;
import java.util.Map;

import javax.naming.NamingException;

import net.morilib.ldap.AttributeFilter;
import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.subr.QuaternaryArgs;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class SelectLdapService extends QuaternaryArgs {

	//
	private Datum tolist(List<Map<String, List<String>>> rs) {
		ConsListBuilder r1, r2, r3;

		r1 = new ConsListBuilder();
		for(Map<String, List<String>> e1 : rs) {
			r2 = new ConsListBuilder();
			for(Map.Entry<String, List<String>> e2 : e1.entrySet()) {
				r3 = new ConsListBuilder();
				for(String e3 : e2.getValue()) {
					r3.append(new LispString(e3));
				}
				r2.append(new Cons(
						new LispString(e2.getKey()), r3.get()));
			}
			r1.append(r2.get());
		}
		return r1.get();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.QuaternaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a, Datum c4a,
			Environment env, LispMessage mesg) {
		String dn;
		AttributeFilter fl;
		int sc;
		List<Map<String, List<String>>> rs;

		if(!(c1a instanceof LispLDAPService)) {
			throw mesg.getError("err.ldap.require.ldapservice", c1a);
		} else try {
			dn = SubrUtils.getString(c2a, mesg);
			fl = LispLDAPService.parseFilter(c3a, mesg);
			sc = LispLDAPService.parseScope(c4a, mesg);
			rs = ((LispLDAPService)c1a).manager.search(dn, fl, sc);
			return tolist(rs);
		} catch (NamingException e) {
			throw mesg.getError("err.ldap.error.naming");
		}
	}

}
