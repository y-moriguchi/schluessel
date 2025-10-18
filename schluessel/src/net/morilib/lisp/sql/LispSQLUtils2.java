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

import java.sql.ParameterMetaData;
import java.sql.PreparedStatement;
import java.sql.SQLException;
import java.sql.Types;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.datetime.LispTime;
import net.morilib.sql.util.DBMSDependentException;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/11
 */
public class LispSQLUtils2 extends LispSQLUtils {

	/**
	 * @param dbms
	 */
	protected LispSQLUtils2(String dbms) {
		super(dbms);
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
	protected void doSetArray(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetBigInt(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.setBigDecimal(column, d.getBigDecimal());
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
	protected void doSetBinary(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetBit(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.setBoolean(column, d.isTrue());
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
	protected void doSetBlob(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetBoolean(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.setBoolean(column, d.isTrue());
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
	protected void doSetChar(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.setString(column, d.getString());
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
	protected void doSetClob(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetDatalink(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetDate(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispDate) {
			rs.setDate(column,
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
	protected void doSetDecimal(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.setBigDecimal(column, d.getBigDecimal());
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
	protected void doSetDistinct(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetDouble(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.setDouble(column, d.getRealDouble());
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
	protected void doSetFloat(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.setFloat(column, (float)d.getRealDouble());
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
	protected void doSetInteger(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.setInt(column, d.getInt());
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
	protected void doSetJavaDatum(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetLongvarbinary(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetLongvarchar(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetNull(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetNumeric(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.setBigDecimal(column, d.getBigDecimal());
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
	protected void doSetOther(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetReal(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispReal) {
			rs.setDouble(column, d.getRealDouble());
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
	protected void doSetRef(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetSmallint(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispInteger) {
			rs.setShort(column, (short)d.getInt());
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
	protected void doSetStruct(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetTime(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispTime) {
			rs.setTime(column,
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
	protected void doSetTimestamp(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		if(d instanceof LispDate) {
			rs.setTimestamp(column,
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
	protected void doSetTinyint(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetVarbinary(
			PreparedStatement rs, ParameterMetaData md,
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
	protected void doSetVarchar(
			PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		rs.setString(column, d.getString());
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
	protected void doSetType(
			int type, PreparedStatement rs, ParameterMetaData md,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		throw new DBMSDependentException();
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lisp.sql.LispSQLUtils#setParameter(java.sql.PreparedStatement, int, net.morilib.lisp.Datum)
	 */
	public void setParameter(
			PreparedStatement ps,
			int column,
			Datum d) throws SQLException, DBMSDependentException {
		ParameterMetaData md = ps.getParameterMetaData();
	
		switch(md.getParameterType(column)) {
		case Types.ARRAY:
			doSetArray(ps, md, column, d);
			break;
		case Types.BIGINT:
			doSetBigInt(ps, md, column, d);
			break;
		case Types.BINARY:
			doSetBinary(ps, md, column, d);
			break;
		case Types.BIT:
			doSetBit(ps, md, column, d);
			break;
		case Types.BLOB:
			doSetBlob(ps, md, column, d);
			break;
		case Types.BOOLEAN:
			doSetBoolean(ps, md, column, d);
			break;
		case Types.CHAR:
			doSetChar(ps, md, column, d);
			break;
		case Types.CLOB:
			doSetClob(ps, md, column, d);
			break;
		case Types.DATALINK:
			doSetDatalink(ps, md, column, d);
			break;
		case Types.DATE:
			doSetDate(ps, md, column, d);
			break;
		case Types.DECIMAL:
			doSetDecimal(ps, md, column, d);
			break;
		case Types.DISTINCT:
			doSetDistinct(ps, md, column, d);
			break;
		case Types.DOUBLE:
			doSetDouble(ps, md, column, d);
			break;
		case Types.FLOAT:
			doSetFloat(ps, md, column, d);
			break;
		case Types.INTEGER:
			doSetInteger(ps, md, column, d);
			break;
		case Types.JAVA_OBJECT:
			doSetJavaDatum(ps, md, column, d);
			break;
		case Types.LONGVARBINARY:
			doSetLongvarbinary(ps, md, column, d);
			break;
		case Types.LONGVARCHAR:
			doSetLongvarchar(ps, md, column, d);
			break;
		case Types.NULL:
			doSetNull(ps, md, column, d);
			break;
		case Types.NUMERIC:
			doSetNumeric(ps, md, column, d);
			break;
		case Types.OTHER:
			doSetOther(ps, md, column, d);
			break;
		case Types.REAL:
			doSetReal(ps, md, column, d);
			break;
		case Types.REF:
			doSetRef(ps, md, column, d);
			break;
		case Types.SMALLINT:
			doSetSmallint(ps, md, column, d);
			break;
		case Types.STRUCT:
			doSetStruct(ps, md, column, d);
			break;
		case Types.TIME:
			doSetTime(ps, md, column, d);
			break;
		case Types.TIMESTAMP:
			doSetTimestamp(ps, md, column, d);
			break;
		case Types.TINYINT:
			doSetTinyint(ps, md, column, d);
			break;
		case Types.VARBINARY:
			doSetVarbinary(ps, md, column, d);
			break;
		case Types.VARCHAR:
			doSetVarchar(ps, md, column, d);
			break;
		default:
			doSetType(
					md.getParameterType(column),
					ps, md, column, d);
			break;
		}
	}

}
