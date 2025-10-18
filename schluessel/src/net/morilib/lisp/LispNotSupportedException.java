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

public class LispNotSupportedException extends LispException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -6152056944582467554L;

	/**
	 * 
	 */
	public LispNotSupportedException() {
		this("this feature is not supported in this version");
	}

	/**
	 * @param message
	 */
	public LispNotSupportedException(String message) {
		super("err.notsupport", message,
				LispSimpleCondition.newInstance(
						"&implementation-restriction"));
	}

}
