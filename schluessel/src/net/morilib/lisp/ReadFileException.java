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

import net.morilib.lisp.condition.LispSimpleCondition;

public class ReadFileException extends LispException {

	//
	private static final long serialVersionUID = -2680644380972425554L;
	private static final String ERR_CODE = "err.readfile";

	/**
	 * 
	 */
	public ReadFileException() {
		super(ERR_CODE);
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ReadFileException(String message, Throwable cause) {
		super(ERR_CODE, message,
				LispSimpleCondition.newInstance("&violation"), cause);
	}

	/**
	 * @param message
	 */
	public ReadFileException(String message) {
		super(ERR_CODE, message,
				LispSimpleCondition.newInstance("&violation"));
	}

	/**
	 * @param cause
	 */
	public ReadFileException(Throwable cause) {
		super(ERR_CODE,
				LispSimpleCondition.newInstance("&violation"), cause);
	}

}
