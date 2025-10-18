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
package net.morilib.parser;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/13
 */
public class ParserReflectionException extends RuntimeException {

	//
	private static final long serialVersionUID = 8053084181306989316L;

	/**
	 * 
	 */
	public ParserReflectionException() {
		super();
	}

	/**
	 * @param message
	 * @param cause
	 */
	public ParserReflectionException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public ParserReflectionException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public ParserReflectionException(Throwable cause) {
		super(cause);
	}

}
