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
package net.morilib.lisp.xml;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/20
 */
public class XMLParseException extends Exception {

	//
	private static final long serialVersionUID = -7694057188617590686L;

	/**
	 * 
	 */
	public XMLParseException() {
		super();
	}

	/**
	 * @param message
	 * @param cause
	 */
	public XMLParseException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public XMLParseException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public XMLParseException(Throwable cause) {
		super(cause);
	}

}
