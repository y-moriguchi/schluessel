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
package net.morilib.lang.composite;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/09
 */
public class FunctorClassContractException extends FunctorException {

	//
	private static final long serialVersionUID = 5361245050149986059L;

	/**
	 * 
	 */
	public FunctorClassContractException() {
		super();
	}

	/**
	 * @param message
	 * @param cause
	 */
	public FunctorClassContractException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * @param message
	 */
	public FunctorClassContractException(String message) {
		super(message);
	}

	/**
	 * @param cause
	 */
	public FunctorClassContractException(Throwable cause) {
		super(cause);
	}

}
