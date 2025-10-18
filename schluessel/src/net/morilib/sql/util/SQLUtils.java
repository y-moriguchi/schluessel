/*
 * Copyright 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package net.morilib.sql.util;

import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/06
 */
public class SQLUtils {

	//
	private String dbms;

	/**
	 * 
	 */
	protected SQLUtils(String dbms) {
		this.dbms = dbms;
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doArray(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return getObjects(rs.getArray(column).getResultSet());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doBigInt(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBigDecimal(column).toBigInteger();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doBinary(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBytes(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doBit(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBoolean(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doBlob(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doBoolean(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBoolean(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doChar(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getString(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doClob(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doDatalink(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doDate(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new java.util.Date(rs.getDate(column).getTime());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doDecimal(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBigDecimal(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doDistinct(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doDouble(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getDouble(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doFloat(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getFloat(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doInteger(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getInt(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doJavaObject(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doLongvarbinary(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doLongvarchar(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doNull(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return null;
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doNumeric(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBigDecimal(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doOther(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doReal(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getDouble(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doRef(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doSmallint(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getInt(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doStruct(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doTime(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new java.util.Date(rs.getDate(column).getTime());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doTimestamp(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new java.util.Date(rs.getDate(column).getTime());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doTinyint(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getInt(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doVarbinary(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getBytes(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doVarchar(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return rs.getString(column);
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Object doType(
			int type, ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	public Object getObject(
			ResultSet rs,
			int column) throws SQLException, DBMSDependentException {
		ResultSetMetaData md = rs.getMetaData();

		switch(md.getColumnType(column)) {
		case Types.ARRAY:
			return doArray(rs, md, column);
		case Types.BIGINT:
			return doBigInt(rs, md, column);
		case Types.BINARY:
			return doBinary(rs, md, column);
		case Types.BIT:
			return doBit(rs, md, column);
		case Types.BLOB:
			return doBlob(rs, md, column);
		case Types.BOOLEAN:
			return doBoolean(rs, md, column);
		case Types.CHAR:
			return doChar(rs, md, column);
		case Types.CLOB:
			return doClob(rs, md, column);
		case Types.DATALINK:
			return doDatalink(rs, md, column);
		case Types.DATE:
			return doDate(rs, md, column);
		case Types.DECIMAL:
			return doDecimal(rs, md, column);
		case Types.DISTINCT:
			return doDistinct(rs, md, column);
		case Types.DOUBLE:
			return doDouble(rs, md, column);
		case Types.FLOAT:
			return doFloat(rs, md, column);
		case Types.INTEGER:
			return doInteger(rs, md, column);
		case Types.JAVA_OBJECT:
			return doJavaObject(rs, md, column);
		case Types.LONGVARBINARY:
			return doLongvarbinary(rs, md, column);
		case Types.LONGVARCHAR:
			return doLongvarchar(rs, md, column);
		case Types.NULL:
			return doNull(rs, md, column);
		case Types.NUMERIC:
			return doNumeric(rs, md, column);
		case Types.OTHER:
			return doOther(rs, md, column);
		case Types.REAL:
			return doReal(rs, md, column);
		case Types.REF:
			return doRef(rs, md, column);
		case Types.SMALLINT:
			return doSmallint(rs, md, column);
		case Types.STRUCT:
			return doStruct(rs, md, column);
		case Types.TIME:
			return doTime(rs, md, column);
		case Types.TIMESTAMP:
			return doTimestamp(rs, md, column);
		case Types.TINYINT:
			return doTinyint(rs, md, column);
		case Types.VARBINARY:
			return doVarbinary(rs, md, column);
		case Types.VARCHAR:
			return doVarchar(rs, md, column);
		default:
			return doType(md.getColumnType(column), rs, md, column);
		}
	}

	/**
	 * 
	 * @param rs
	 * @return
	 * @throws SQLException
	 */
	public Object[] getObjects(
			ResultSet rs) throws SQLException, DBMSDependentException {
		ResultSetMetaData md = rs.getMetaData();
		Object[] res = new Object[md.getColumnCount()];

		for(int i = 0; i < res.length; i++) {
			res[i] = getObject(rs, i);
		}
		return res;
	}

	/**
	 * @return the dbms
	 */
	public String getDbms() {
		return dbms;
	}

}
