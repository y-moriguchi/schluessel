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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import net.morilib.lisp.Datum;
import net.morilib.sql.util.DBMSDependentException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/05
 */
public class LispJdbcPrepared
extends Datum implements LispJdbcClosable {

	//
	private PreparedStatement prepared;
	private boolean closed = false;

	/**
	 * @param connection
	 * @param string
	 * @throws SQLException 
	 */
	public LispJdbcPrepared(
			Connection connection, String sql) throws SQLException {
		prepared = connection.prepareStatement(sql);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sql.LispJdbcClosable#isClosed()
	 */
	public boolean isClosed() {
		return closed;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sql.LispJdbcClosable#close()
	 */
	public void close() throws SQLException {
		if(!closed) {
			prepared.close();
			prepared = null;
			closed = true;
		}
	}

	/**
	 * @param i
	 * @param datum
	 * @throws DBMSDependentException 
	 * @throws SQLException 
	 */
	public void setDatum(
			int i,
			Datum d) throws SQLException, DBMSDependentException {
		if(!closed) {
			LispSQLUtils.getSQLUtils().setParameter(prepared, i, d);
		}
	}

	/**
	 * @return
	 * @throws SQLException 
	 */
	public ResultSet query() throws SQLException {
		return closed ? null : prepared.executeQuery();
	}

	/**
	 * @return
	 */
	public int update() throws SQLException {
		return closed ? 0 : prepared.executeUpdate();
	}

}
