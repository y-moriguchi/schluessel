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

import java.lang.reflect.Method;
import java.util.List;

/*package*/ class JavaMethod extends Subr {
	
	//
	private Method mthdesc;
	
	
	/*package*/ JavaMethod(String name, Method pd) {
		this.symbolName = name;
		this.mthdesc    = pd;
	}
	
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		if(body instanceof Cons) {
			Cons        c0  = (Cons)body;
			List<Datum> lst = LispUtils.consToList(c0.getCdr(), mesg);
			
			if(!(c0.getCar() instanceof JavaInstance)) {
				throw mesg.getError(
						"err.require.java-bean",
						c0.getCar());
			}
			
			try {
				JavaInstance bn = (JavaInstance)c0.getCar();
				Object       jo;
				
				jo = JavaUtils.invokeMethod(
						mthdesc,
						bn.getJavaInstance(),
						lst);
				return LispUtils.toDatum(jo);
			} catch (ParameterNotFoundException e) {
				throw mesg.getError(
						"err.java.method.notfound");
			}
		}
		throw mesg.getError("err.argument");
	}

}
