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

public abstract class ToLispException extends Exception {
	
	/**
	 * 
	 */
	private static final long serialVersionUID = -2193389730171507465L;
	
	private Datum datum = null;
	

	/**
	 * @param message
	 */
	public ToLispException(String message) {
		super(message);
	}

	/**
	 * @param message
	 */
	public ToLispException(String message, Datum d) {
		super(message);
		datum = d;
	}

	/**
	 * @return the datum
	 */
	public Datum getDatum() {
		return datum;
	}
	
	
	public LispException toLispException(LispMessage mesg) {
		if(datum == null) {
			return mesg.getError(getMessage());
		} else {
			return mesg.getError(getMessage(), datum);
		}
	}
	
}
