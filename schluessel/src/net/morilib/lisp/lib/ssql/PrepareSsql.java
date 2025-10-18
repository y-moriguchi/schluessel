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
package net.morilib.lisp.lib.ssql;

import java.sql.SQLException;
import java.util.Map;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Undef;
import net.morilib.lisp.sql.LispJdbcPrepared;
import net.morilib.lisp.sql.LispSQLException;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.sql.util.DBMSDependentException;
import net.morilib.util.ArrayListStack;
import net.morilib.util.Stack2;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/02
 */
public class PrepareSsql extends TernaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Datum c3a,
			Environment env, LispMessage mesg) {
		LispJdbcPrepared pp;
		Stack2<Datum> st = new ArrayListStack<Datum>();
		Map<Datum, Datum> mp;
		Datum d1, d2;
		int no = 1;

		if(c1a instanceof LispJdbcPrepared) {
			pp = (LispJdbcPrepared)c1a;
		} else {
			throw mesg.getError("err.jdbc.require.prepared", c1a);
		}

		mp = LispUtils.assocToMap(c3a);
		st.add(c2a);
		while(!st.isEmpty()) {
			if((d1 = st.pop()) instanceof Cons) {
				st.push(((Cons)d1).getCdr());
				st.push(((Cons)d1).getCar());
			} else if(d1 instanceof Keyword &&
					(d2 = mp.get(d1)) != null) {
				try {
					pp.setDatum(no++, d2);
				} catch (SQLException e) {
					throw LispSQLException.getError(mesg, e);
				} catch (DBMSDependentException e) {
					throw mesg.getError("err.jdbc.typemapping", d2);
				}
			}
		}
		return Undef.UNDEF;
	}

}
