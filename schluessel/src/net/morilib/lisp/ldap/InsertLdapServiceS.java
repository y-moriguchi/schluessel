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

import javax.naming.NamingException;
import javax.naming.directory.Attributes;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.TernaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/15
 */
public class InsertLdapServiceS extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.TernaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		String dn;
		Attributes as;

		if(!(c1a instanceof LispLDAPService)) {
			throw mesg.getError("err.ldap.require.ldapservice", c1a);
		} else try {
			dn = SubrUtils.getString(c2a, mesg);
			as = LispLDAPService.parseAttributes(c3a, mesg);
			((LispLDAPService)c1a).manager.add(dn, as);
			return Undef.UNDEF;
		} catch (NamingException e) {
			throw mesg.getError("err.ldap.error.naming");
		}
	}

}
