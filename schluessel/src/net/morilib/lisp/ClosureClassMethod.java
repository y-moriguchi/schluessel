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

/*package*/ final class ClosureClassMethod extends Datum {
	
	private Datum params;
	private CompiledCode body;
	private Datum typelist = null;
	
	
	/*package*/ ClosureClassMethod(
			Datum params, CompiledCode body, Datum typelist) {
		if(params == null || body == null || typelist == null) {
			throw new NullPointerException();
		}
		this.params   = params;
		this.body     = body;
		this.typelist = typelist;
	}
	
	
	/*package*/ Datum getParameterList() {
		return params;
	}
	
	
	/*package*/ CompiledCode getCode() {
		return body;
	}
	
	
	/*package*/ Datum getTypeList() {
		return typelist;
	}
	
	
	public boolean isTypeProcedure() {
		return true;
	}
	
}
