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

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.relation.LispEmptyResultBag;
import net.morilib.lisp.sql.LispJdbcPrepared;
import net.morilib.lisp.sql.LispJdbcResultBag;
import net.morilib.lisp.sql.LispSQLException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/05
 */
public class JdbcExecuteQuery extends Subr {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		List<Datum> l = LispUtils.consToList(body, mesg);
		LispJdbcPrepared lps;
		ResultSet rs;

		if(l.size() != 1) {
			throw mesg.getError("err.argument", body);
		} else if(!(l.get(0) instanceof LispJdbcPrepared)) {
			throw mesg.getError("err.jdbc.require.prepared", l.get(0));
		}
		lps = (LispJdbcPrepared)l.get(0);

		try {
			rs = lps.query();
			if(rs != null && rs.next()) {
				return new LispJdbcResultBag(rs);
			} else {
				rs.close();
				return LispEmptyResultBag.INSTANCE;
			}
		} catch (SQLException e) {
			throw LispSQLException.getError(mesg, e);
		}
	}

}
