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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class Macro extends Datum {

	//
	//private Environment env;
	//private Datum params;
	//private CompiledCode body;
	private Closure mclosure;

	//
	/*package*/ Macro(Closure cls) {
		if(cls == null) {
			throw new NullPointerException();
		}
		this.mclosure = cls;
	}

	//
	/*package*/ /*Macro(Datum params, CompiledCode body, Environment env) {
		if(params == null || body == null || env == null) {
			throw new NullPointerException();
		}
		this.params = params;
		this.body = body;
		this.env = env;
	}*/

	//
	/*package*/ Closure getClosure() {
		return mclosure;
	}

	/*package*/ /*Datum getParameterList() {
		return params;
	}*/

	/*package*/ /*CompiledCode getCode() {
		return body;
	}*/

	/*package*/ /*Environment getEnvironment() {
		return env;
	}*/

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<macro>");
	}

}

