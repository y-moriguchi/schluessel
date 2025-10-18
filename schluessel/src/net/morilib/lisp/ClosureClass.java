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
 * @author MORIGUCHI, Yuichiro 2011/04/09
 */
/*package*/ final class ClosureClass extends Datum {

	//
	private Datum params;
	private CompiledCode body;

	//
	/*package*/ ClosureClass() {
		// do nothing
	}

	//
	/*package*/ ClosureClass(Datum params, CompiledCode body) {
		if(params == null || body == null) {
			throw new NullPointerException();
		}
		this.params   = params;
		this.body     = body;
	}

	//
	/*package*/ Datum getParameterList() {
		return params;
	}

	//
	/*package*/ CompiledCode getCode() {
		return body;
	}

	/**
	 * @param params the params to set
	 */
	/*package*/ void setParameterList(Datum params) {
		this.params = params;
	}

	/**
	 * @param body the body to set
	 */
	/*package*/ void setCode(CompiledCode body) {
		this.body = body;
	}

	//
	public boolean isTypeProcedure() {
		return true;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<closureClass" +
				Integer.toString(System.identityHashCode(this), 16) +
				">");
	}

}
