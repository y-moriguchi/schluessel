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
package net.morilib.lisp.sql;

import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispDouble;
import net.morilib.sql.util.DBMSDependentException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/11
 */
/*package*/ class LispOracleUtils extends LispSQLUtils {

	/**
	 * @param dbms
	 */
	public LispOracleUtils(String dbms) {
		super(dbms);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sql.LispSQLUtils#doGetType(int, java.sql.ResultSet, java.sql.ResultSetMetaData, int)
	 */
	@Override
	protected Datum doGetType(
			int type, ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		switch(type) {
		case 100:  // BINARY_FLOAT
			return new LispDouble(rs.getDouble(column));
		case 101:  // BINARY_DOUBLE
			return new LispDouble(rs.getDouble(column));
		}
		return super.doGetType(type, rs, md, column);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sql.LispSQLUtils#doSetBoolean(java.sql.PreparedStatement, int, java.lang.Boolean)
	 */
	@Override
	protected void doSetBoolean(PreparedStatement ps, int column, Boolean o)
			throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

}
