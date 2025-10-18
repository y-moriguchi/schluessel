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

import java.sql.SQLException;

import net.morilib.lisp.LispException;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.condition.LispCompoundCondition;
import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispSimpleCondition;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/05
 */
public class LispSQLException extends LispException {

	//
	private static final long serialVersionUID = 1349654199831740484L;
	private static final String ERR_CD = "err.jdbc.sqlerror";

	//
	private SQLException sqlException;

	//
	private LispSQLException(
			String code, String message, SQLException cause) {
		super(code, message, makecond(message), cause);
		this.sqlException = cause;
	}

	/**
	 * 
	 * @param mesg
	 * @param cause
	 * @return
	 */
	public static LispSQLException getError(
			LispMessage mesg, SQLException cause) {
		return new LispSQLException(ERR_CD, mesg.get(ERR_CD), cause);
	}

	//
	private static LispCondition makecond(String message) {
		LispCondition err;
		LispSimpleCondition msg;

		err = LispMessage.getConditionByKey(ERR_CD);
		msg = LispSimpleCondition.newInstance("&message");
		msg.setField("message", new LispString(message));
		return new LispCompoundCondition(err, msg);
	}

	//
	/*package*/ LispSQLException getNextException(LispMessage mesg) {
		return getError(mesg, sqlException.getNextException());
	}

	/**
	 * 
	 */
	public int getSQLErrorCode() {
		return sqlException.getErrorCode();
	}

	/**
	 * 
	 * @return
	 */
	public String getSQLState() {
		return sqlException.getSQLState();
	}

	/**
	 * 
	 * @return
	 */
	public SQLException getException() {
		return sqlException;
	}

}
