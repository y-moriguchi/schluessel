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

public class ReadException extends LispException {

	//
	private static final long serialVersionUID = 4625758232366686685L;

	/**
	 * @param code
	 * @param message
	 */
	public ReadException(String code, String message,
			LispCondition cond) {
		super(code, message, cond);
	}

	/**
	 * @param code
	 */
	public ReadException(String code) {
		super(code);
	}

}
