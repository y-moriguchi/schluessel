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
package net.morilib.util;

/**
 * Exception which raises if the number is out of bounds.
 * <p>自然数系の表現の境界を越えたときに発生する例外である。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public class InclementorBoundsException extends RuntimeException {

	//
	private static final long serialVersionUID = 7066317476985444026L;

	/**
	 * constructs an exception.
	 * <p>例外を生成する。
	 */
	public InclementorBoundsException() {
		super();
	}

	/**
	 * constructs an exception.
	 * <p>例外を生成する。
	 * 
	 * @param message message
	 */
	public InclementorBoundsException(String message) {
		super(message);
	}

}
