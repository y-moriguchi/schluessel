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

import net.morilib.lisp.condition.LispCompoundCondition;
import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispSimpleCondition;

public class UserException extends LispException {

	//
	private static final long serialVersionUID = -3161502249690457404L;

	/**
	 * 
	 */
	public UserException() {
		super("err.user");
	}

	/**
	 * @param message
	 */
	public UserException(String message) {
		super("err.user", message, makecond(message));
	}

	//
	private static LispCondition makecond(String message) {
		LispSimpleCondition err, msg;

		err = LispSimpleCondition.newInstance("&non-continuable");
		msg = LispSimpleCondition.newInstance("&message");
		msg.setField("message", new LispString(message));
		return new LispCompoundCondition(err, msg);
	}

}
