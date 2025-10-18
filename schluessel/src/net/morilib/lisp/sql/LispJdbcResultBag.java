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

import java.sql.ResultSet;
import java.sql.SQLException;

import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.relation.LispResultBag;
import net.morilib.lisp.relation.LispRelationException;
import net.morilib.lisp.relation.LispDBRow;
import net.morilib.sql.util.DBMSDependentException;

/**
 *
 * NOTE: This class is NOT thread-safe.
 * 
 * @author MORIGUCHI, Yuichiro 2011/02/06
 */
public class LispJdbcResultBag extends LispResultBag
implements LispJdbcClosable {

	//
	private class Row extends LispDBRow {

		//
		private Datum[] data;

		//
		private Row() throws LispRelationException {
			try {
				LispSQLUtils utils = LispSQLUtils.getSQLUtils();
				int arity = resultSet.getMetaData().getColumnCount();

				data = new Datum[arity];
				for(int i = 0; i < arity; i++) {
					data[i] = utils.getDatum(resultSet, i + 1);
				}
			} catch(SQLException e) {
				throw new LispRelationException(e);
			} catch (DBMSDependentException e) {
				throw new LispRelationException(e);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.relation.LispRelationRow#get(java.lang.Object)
		 */
		@Override
		public Datum get(Object column) throws LispRelationException {
			try {
				return data[resultSet.findColumn(
						column.toString()) - 1];
			} catch (SQLException e) {
				throw new LispRelationException(e);
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.relation.LispRelationRow#set(java.lang.Object, net.morilib.lisp.Datum)
		 */
		@Override
		public void set(Object column,
				Datum value) throws LispRelationException {
			try {
				data[resultSet.findColumn(column.toString()) - 1] =
					value;
			} catch (SQLException e) {
				throw new LispRelationException(e);
			}
			
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
		 */
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<result-bag-row>");
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.relation.LispDBRow#getByList()
		 */
		@Override
		public Datum getByList() throws LispRelationException {
			ConsListBuilder l = new ConsListBuilder();

			for(int i = 0; i < data.length; i++) {
				l.append(data[i]);
			}
			return l.get();
		}

	}

	//
	private ResultSet resultSet;
	private boolean closed = false;

	/**
	 * 
	 * @param rs
	 */
	public LispJdbcResultBag(ResultSet rs) {
		this.resultSet = rs;
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.Relation#isDeletable()
	 */
	public boolean isDeletable() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.Relation#deleteRow(int)
	 */
	public void deleteRow(int row) throws LispRelationException {
		try {
			int r = resultSet.getRow();
	
			resultSet.absolute(row);
			resultSet.deleteRow();
			resultSet.absolute(r);
		} catch(SQLException e) {
			throw new LispRelationException(e);
		}
		
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.Relation#isInsertable()
	 */
	public boolean isInsertable() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.relation.Relation#insertRow(T[])
	 */
	public void insertRow(Datum... data) throws LispRelationException {
		try {
			LispSQLUtils utils = LispSQLUtils.getSQLUtils();

			resultSet.moveToInsertRow();
			for(int i = 0; i < data.length; i++) {
				utils.updateDatum(resultSet, i + 1, data[i]);
			}
			resultSet.moveToCurrentRow();
		} catch(SQLException e) {
			throw new LispRelationException(e);
		} catch (DBMSDependentException e) {
			throw new LispRelationException(e);
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.relation.LispRelation#referRow(int)
	 */
	public LispDBRow referRow(
			int row) throws LispRelationException {
		try {
			int r = resultSet.getRow();
			Row res;
	
			resultSet.absolute(row);
			res = new Row();
			resultSet.absolute(r);
			return res;
		} catch(SQLException e) {
			throw new LispRelationException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.relation.LispDBCursor#first()
	 */
	@Override
	public LispDBRow getRow() throws LispRelationException {
		return new Row();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.relation.LispDBCursor#rest()
	 */
	@Override
	public LispResultBag rest() throws LispRelationException {
		try {
			resultSet.next();
			return this;
		} catch (SQLException e) {
			throw new LispRelationException(e);
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.relation.LispResultBag#isLast()
	 */
	@Override
	public boolean isAfterLast() throws LispRelationException {
		try {
			return resultSet.isAfterLast();
		} catch (SQLException e) {
			throw new LispRelationException(e);
		}
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
		resultSet.close();
		closed = true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<result-bag>");
	}

}
