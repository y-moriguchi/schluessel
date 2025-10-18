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
package net.morilib.util.codec;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/23
 */
public class DecodeException extends Exception {

	/**
	 * 
	 */
	public DecodeException() {
		super();
	}

	/**
	 * @param message
	 * @param cause
	 */
	public DecodeException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public DecodeException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public DecodeException(Throwable cause) {
		super(cause);
	}

}
