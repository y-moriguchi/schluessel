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

public class LispArithmeticException extends LispException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -8977322745873936726L;

	/**
	 * 
	 */
	public LispArithmeticException(String code) {
		super(code);
	}

	/**
	 * @param message
	 */
	public LispArithmeticException(String code, String message,
			LispCondition condition) {
		super(code, message, condition);
	}

}
