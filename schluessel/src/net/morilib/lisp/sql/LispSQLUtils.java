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
package net.morilib.lisp.sql;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.ResultSetMetaData;
import java.sql.SQLException;
import java.sql.Types;
import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispDouble;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispString;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.LispVector;
import net.morilib.lisp.Nil;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.datetime.LispTime;
import net.morilib.sql.util.DBMSDependentException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/06
 */
public class LispSQLUtils {

	//
	private static LispSQLUtils defaultSQLUtils;
	private static Map<String, LispSQLUtils> sqlUtils;

	//
	private String dbms;

	static {
		sqlUtils = new HashMap<String, LispSQLUtils>();
		defaultSQLUtils = new LispSQLUtils("default");
	}

	/**
	 * 
	 */
	protected LispSQLUtils(String dbms) {
		this.dbms = dbms;
		sqlUtils.put(dbms, this);
	}

	/**
	 * 
	 * @return
	 */
	public static LispSQLUtils getSQLUtils() {
		return defaultSQLUtils;
	}

	/**
	 * @param name
	 */
	public static void setSQLUtils(String name) {
		LispSQLUtils su = sqlUtils.get(name);

		defaultSQLUtils = (su == null) ? sqlUtils.get("default") : su;
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetArray(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispVector(
				getData(rs.getArray(column).getResultSet()));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetBigInt(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispInteger.valueOf(
				rs.getBigDecimal(column).toBigInteger());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetBinary(
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
	protected Datum doGetBit(
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
	protected Datum doGetBlob(
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
	protected Datum doGetBoolean(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispBoolean.getInstance(rs.getBoolean(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetChar(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispString(rs.getString(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetClob(
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
	protected Datum doGetDatalink(
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
	protected Datum doGetDate(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispDate(
				new java.util.Date(rs.getDate(column).getTime()));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetDecimal(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispUtils.bigDecimalToRational(
				rs.getBigDecimal(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetDistinct(
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
	protected Datum doGetDouble(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispDouble(rs.getDouble(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetFloat(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispDouble(rs.getFloat(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetInteger(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispInteger.valueOf(rs.getInt(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetJavaDatum(
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
	protected Datum doGetLongvarbinary(
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
	protected Datum doGetLongvarchar(
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
	protected Datum doGetNull(
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
	protected Datum doGetNumeric(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispUtils.bigDecimalToRational(
				rs.getBigDecimal(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetOther(
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
	protected Datum doGetReal(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispDouble(rs.getDouble(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetRef(
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
	protected Datum doGetSmallint(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispInteger.valueOf(rs.getInt(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetStruct(
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
	protected Datum doGetTime(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispTime(
				LispTime.TimeType.TIME_DURATION,
				rs.getDate(column).getTime());
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetTimestamp(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispDate(
				new java.util.Date(rs.getTimestamp(column).getTime()));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetTinyint(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return LispInteger.valueOf(rs.getInt(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetVarbinary(
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
	protected Datum doGetVarchar(
			ResultSet rs, ResultSetMetaData md,
			int column) throws SQLException, DBMSDependentException {
		return new LispString(rs.getString(column));
	}

	/**
	 * 
	 * @param rs
	 * @param column
	 * @return
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected Datum doGetType(
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
	public Datum getDatum(
			ResultSet rs,
			int column) throws SQLException, DBMSDependentException {
		ResultSetMetaData md = rs.getMetaData();
		int type = md.getColumnType(column);

		if(rs.getObject(column) == null) {
			return Nil.NIL;
		}

		switch(type) {
		case Types.ARRAY:
			return doGetArray(rs, md, column);
		case Types.BIGINT:
			return doGetBigInt(rs, md, column);
		case Types.BINARY:
			return doGetBinary(rs, md, column);
		case Types.BIT:
			return doGetBit(rs, md, column);
		case Types.BLOB:
			return doGetBlob(rs, md, column);
		case Types.BOOLEAN:
			return doGetBoolean(rs, md, column);
		case Types.CHAR:
			return doGetChar(rs, md, column);
		case Types.CLOB:
			return doGetClob(rs, md, column);
		case Types.DATALINK:
			return doGetDatalink(rs, md, column);
		case Types.DATE:
			return doGetDate(rs, md, column);
		case Types.DECIMAL:
			return doGetDecimal(rs, md, column);
		case Types.DISTINCT:
			return doGetDistinct(rs, md, column);
		case Types.DOUBLE:
			return doGetDouble(rs, md, column);
		case Types.FLOAT:
			return doGetFloat(rs, md, column);
		case Types.INTEGER:
			return doGetInteger(rs, md, column);
		case Types.JAVA_OBJECT:
			return doGetJavaDatum(rs, md, column);
		case Types.LONGVARBINARY:
			return doGetLongvarbinary(rs, md, column);
		case Types.LONGVARCHAR:
			return doGetLongvarchar(rs, md, column);
		case Types.NULL:
			return doGetNull(rs, md, column);
		case Types.NUMERIC:
			return doGetNumeric(rs, md, column);
		case Types.OTHER:
			return doGetOther(rs, md, column);
		case Types.REAL:
			return doGetReal(rs, md, column);
		case Types.REF:
			return doGetRef(rs, md, column);
		case Types.SMALLINT:
			return doGetSmallint(rs, md, column);
		case Types.STRUCT:
			return doGetStruct(rs, md, column);
		case Types.TIME:
			return doGetTime(rs, md, column);
		case Types.TIMESTAMP:
			return doGetTimestamp(rs, md, column);
		case Types.TINYINT:
			return doGetTinyint(rs, md, column);
		case Types.VARBINARY:
			return doGetVarbinary(rs, md, column);
		case Types.VARCHAR:
			return doGetVarchar(rs, md, column);
		default:
			return doGetType(type, rs, md, column);
		}
	}

	/**
	 * 
	 * @param rs
	 * @return
	 * @throws SQLException
	 */
	public Datum[] getData(
			ResultSet rs) throws SQLException, DBMSDependentException {
		ResultSetMetaData md = rs.getMetaData();
		Datum[] res = new Datum[md.getColumnCount()];

		for(int i = 0; i < res.length; i++) {
			res[i] = getDatum(rs, i + 1);
		}
		return res;
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateArray(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateBigInt(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.updateBigDecimal(column, d.getBigDecimal());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateBinary(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateBit(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.updateBoolean(column, d.isTrue());
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateBlob(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateBoolean(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.updateBoolean(column, d.isTrue());
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateChar(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.updateString(column, d.getString());
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateClob(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateDatalink(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateDate(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispDate) {
			rs.updateDate(column,
					new java.sql.Date(((LispDate)d).getTime()));
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateDecimal(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.updateBigDecimal(column, d.getBigDecimal());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateDistinct(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateDouble(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.updateDouble(column, d.getRealDouble());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateFloat(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.updateFloat(column, (float)d.getRealDouble());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateInteger(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.updateInt(column, d.getInt());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateJavaDatum(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateLongvarbinary(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateLongvarchar(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateNull(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateNumeric(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.updateBigDecimal(column, d.getBigDecimal());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateOther(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateReal(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.updateDouble(column, d.getRealDouble());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateRef(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateSmallint(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.updateShort(column, (short)d.getInt());
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateStruct(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateTime(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispTime) {
			rs.updateTime(column,
					new java.sql.Time(((LispTime)d).getTimeMillis()));
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateTimestamp(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispDate) {
			rs.updateTimestamp(column,
					new java.sql.Timestamp(((LispDate)d).getTime()));
		} else {
			throw new LispSQLObjectException();
		}
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateTinyint(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateVarbinary(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateVarchar(
			ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.updateString(column, d.getString());
	}

	/**
	 * 
	 * @param rs
	 * @param md
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doUpdateType(
			int type, ResultSet rs, ResultSetMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
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
	public void updateDatum(
			ResultSet ps,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		ResultSetMetaData md = ps.getMetaData();

		switch(md.getColumnType(column)) {
		case Types.ARRAY:
			doUpdateArray(ps, md, column, d);
			break;
		case Types.BIGINT:
			doUpdateBigInt(ps, md, column, d);
			break;
		case Types.BINARY:
			doUpdateBinary(ps, md, column, d);
			break;
		case Types.BIT:
			doUpdateBit(ps, md, column, d);
			break;
		case Types.BLOB:
			doUpdateBlob(ps, md, column, d);
			break;
		case Types.BOOLEAN:
			doUpdateBoolean(ps, md, column, d);
			break;
		case Types.CHAR:
			doUpdateChar(ps, md, column, d);
			break;
		case Types.CLOB:
			doUpdateClob(ps, md, column, d);
			break;
		case Types.DATALINK:
			doUpdateDatalink(ps, md, column, d);
			break;
		case Types.DATE:
			doUpdateDate(ps, md, column, d);
			break;
		case Types.DECIMAL:
			doUpdateDecimal(ps, md, column, d);
			break;
		case Types.DISTINCT:
			doUpdateDistinct(ps, md, column, d);
			break;
		case Types.DOUBLE:
			doUpdateDouble(ps, md, column, d);
			break;
		case Types.FLOAT:
			doUpdateFloat(ps, md, column, d);
			break;
		case Types.INTEGER:
			doUpdateInteger(ps, md, column, d);
			break;
		case Types.JAVA_OBJECT:
			doUpdateJavaDatum(ps, md, column, d);
			break;
		case Types.LONGVARBINARY:
			doUpdateLongvarbinary(ps, md, column, d);
			break;
		case Types.LONGVARCHAR:
			doUpdateLongvarchar(ps, md, column, d);
			break;
		case Types.NULL:
			doUpdateNull(ps, md, column, d);
			break;
		case Types.NUMERIC:
			doUpdateNumeric(ps, md, column, d);
			break;
		case Types.OTHER:
			doUpdateOther(ps, md, column, d);
			break;
		case Types.REAL:
			doUpdateReal(ps, md, column, d);
			break;
		case Types.REF:
			doUpdateRef(ps, md, column, d);
			break;
		case Types.SMALLINT:
			doUpdateSmallint(ps, md, column, d);
			break;
		case Types.STRUCT:
			doUpdateStruct(ps, md, column, d);
			break;
		case Types.TIME:
			doUpdateTime(ps, md, column, d);
			break;
		case Types.TIMESTAMP:
			doUpdateTimestamp(ps, md, column, d);
			break;
		case Types.TINYINT:
			doUpdateTinyint(ps, md, column, d);
			break;
		case Types.VARBINARY:
			doUpdateVarbinary(ps, md, column, d);
			break;
		case Types.VARCHAR:
			doUpdateVarchar(ps, md, column, d);
			break;
		default:
			doUpdateType(
					md.getColumnType(column),
					ps, md, column, d);
			break;
		}
	}

	/**
	 * 
	 * @param ps
	 * @param dta
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	public void updateData(
			ResultSet ps,
			Datum... dta) throws SQLException, DBMSDependentException {
		for(int i = 0; i < dta.length; i++) {
			updateDatum(ps, i + 1, dta[i]);
		}
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetString(
			PreparedStatement ps, int column,
			String o) throws SQLException, DBMSDependentException {
		ps.setString(column, (String)o);
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetInteger(
			PreparedStatement ps, int column,
			Integer o) throws SQLException, DBMSDependentException {
		ps.setInt(column, ((Integer)o).intValue());
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetBigInteger(
			PreparedStatement ps, int column,
			BigInteger o) throws SQLException, DBMSDependentException {
		ps.setBigDecimal(
				column, new BigDecimal((BigInteger)o));
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetDate(
			PreparedStatement ps, int column,
			java.util.Date o) throws SQLException, DBMSDependentException {
		try {
			ps.setTimestamp(column, new java.sql.Timestamp(
					((java.util.Date)o).getTime()));
		} catch(SQLException e) {
			ps.setDate(column, new java.sql.Date(
					((java.util.Date)o).getTime()));
		}
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetFloat(
			PreparedStatement ps, int column,
			Float o) throws SQLException, DBMSDependentException {
		try {
			ps.setFloat(column, ((Float)o).floatValue());
		} catch(SQLException e) {
			ps.setDouble(column, ((Float)o).doubleValue());
		}
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetDouble(
			PreparedStatement ps, int column,
			Double o) throws SQLException, DBMSDependentException {
		try {
			ps.setDouble(column, ((Double)o).doubleValue());
		} catch(SQLException e) {
			ps.setFloat(column, ((Double)o).floatValue());
		}
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	protected void doSetBoolean(
			PreparedStatement ps, int column,
			Boolean o) throws SQLException, DBMSDependentException {
		ps.setBoolean(column, ((Boolean)o).booleanValue());
	}

	/**
	 * 
	 * @param ps
	 * @param column
	 * @param d
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	public void setParameter(
			PreparedStatement ps, int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof JavaObjective) {
			Object o = ((JavaObjective)d).toObject();

			if(o == null) {
				ps.setObject(column, null);
			} else if(o instanceof String) {
				doSetString(ps, column, (String)o);
			} else if(o instanceof Integer) {
				doSetInteger(ps, column, (Integer)o);
			} else if(o instanceof BigInteger) {
				doSetBigInteger(ps, column, (BigInteger)o);
			} else if(o instanceof java.util.Date) {
				doSetDate(ps, column, (java.util.Date)o);
			} else if(o instanceof Boolean) {
				doSetBoolean(ps, column, (Boolean)o);
			} else if(o instanceof Float) {
				doSetFloat(ps, column, (Float)o);
			} else if(o instanceof Double) {
				doSetDouble(ps, column, (Double)o);
			} else {
				throw new DBMSDependentException();
			}
		} else {
			throw new DBMSDependentException();
		}
	}

	/**
	 * 
	 * @param ps
	 * @param dta
	 * @throws SQLException
	 * @throws DBMSDependentException
	 */
	public void setParameters(
			PreparedStatement ps,
			Datum... dta) throws SQLException, DBMSDependentException {
		for(int i = 0; i < dta.length; i++) {
			setParameter(ps, i + 1, dta[i]);
		}
	}

	/**
	 * @return the dbms
	 */
	public String getDbms() {
		return dbms;
	}

}
