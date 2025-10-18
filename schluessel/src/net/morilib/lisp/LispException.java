/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.lisp;

import net.morilib.lisp.condition.LispCondition;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class LispException extends RuntimeException {

	//
	private static final long serialVersionUID = 8202334776851614563L;

	//
	private String errorCode;
	private LispCondition condition;

	/**
	 * 
	 */
	public LispException(String code) {
		super();
		this.errorCode = code;
	}

	/**
	 * @param message
	 * @param cause
	 */
	public LispException(
			String code, String message, LispCondition condition,
			Throwable cause) {
		super(message, cause);
		this.errorCode = code;
		this.condition = condition;
	}

	/**
	 * @param message
	 */
	public LispException(String code, String message,
			LispCondition condition) {
		super(message);
		this.errorCode = code;
		this.condition = condition;
	}

	/**
	 * @param cause
	 */
	public LispException(String code, LispCondition condition,
			Throwable cause) {
		super(cause);
		this.errorCode = code;
		this.condition = condition;
	}

	/**
	 * @return the errorCode
	 */
	public String getErrorCode() {
		return errorCode;
	}

	/**
	 * 
	 * @return
	 */
	public Exception getException() {
		return null;
	}

	/**
	 * 
	 * @return
	 */
	public LispCondition getCondition() {
		return condition;
	}

}
