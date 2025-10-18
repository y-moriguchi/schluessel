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
package net.morilib.lisp.jndi;

import java.util.HashMap;
import java.util.Hashtable;
import java.util.Map;
import java.util.Properties;

import javax.sql.DataSource;

import net.morilib.lisp.Cons;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/20
 */
public class LispDatasource extends Datum2 {

	//
	DataSource source;

	/**
	 * 
	 * @param source
	 */
	public LispDatasource(DataSource source) {
		this.source = source;
	}

	//
	private static final Map<Datum, String> SYM;

	//
	static {
		SYM = new HashMap<Datum, String>();
		SYM.put(Symbol.getSymbol("applet"),
				"java.naming.applet");
		SYM.put(Symbol.getSymbol("authoritative"),
				"java.naming.authoritative");
		SYM.put(Symbol.getSymbol("batchsize"),
				"java.naming.batchsize");
		SYM.put(Symbol.getSymbol("dns-url"),
				"java.naming.dns.url");
		SYM.put(Symbol.getSymbol("initial-context-factory"),
				"java.naming.factory.initial");
		SYM.put(Symbol.getSymbol("language"),
				"java.naming.language");
		SYM.put(Symbol.getSymbol("object-factories"),
				"java.naming.factory.object");
		SYM.put(Symbol.getSymbol("provider-url"),
				"java.naming.provider.url");
		SYM.put(Symbol.getSymbol("referral"),
				"java.naming.referral");
		SYM.put(Symbol.getSymbol("security-authentication"),
				"java.naming.security.authentication");
		SYM.put(Symbol.getSymbol("security-credentials"),
				"java.naming.security.credentials");
		SYM.put(Symbol.getSymbol("security-principal"),
				"java.naming.security.principal");
		SYM.put(Symbol.getSymbol("security-protocol"), 
				"java.naming.security.protocol");
		SYM.put(Symbol.getSymbol("state-factories"),
				"java.naming.factory.state");
		SYM.put(Symbol.getSymbol("url-pkg-prefixes"),
				"java.naming.factory.url.pkgs");
	}

	//
	static Hashtable<?, ?> nextProp(Datum d, LispMessage m) {
		ConsIterator itr;
		Properties r = System.getProperties();
		String s, t;
		Datum d2;

		if(d == null)  return null;
		itr = new ConsIterator(d);
		while(itr.hasNext()) {
			if(!((d2 = itr.next()) instanceof Cons)) {
				throw m.getError("err.require.pair");
			}
			t = SubrUtils.getString(((Cons)d2).getCdr(), m);
			if((s = SYM.get(((Cons)d2).getCar())) == null) {
				s = SubrUtils.getString(((Cons)d2).getCar(), m);
			}
			r.put(s, t);
		}
		SubrUtils.checkProper(itr, d, m);
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<db-datasource>");
	}

}
