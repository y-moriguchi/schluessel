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

import java.util.List;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ class JavaClass extends Datum {
	
	//
	private Class<?> klass;
	
	//
	/*package*/ JavaClass(String name) throws ClassNotFoundException {
		if(name == null) {
			throw new NullPointerException();
		}
		this.klass = Class.forName(name);
	}
	
	//
	/*package*/ Class<?> getJavaClass() {
		return klass;
	}
	
	//
	/*package*/ JavaInstance newJavaInstance(
			List<Datum> lst) throws ParameterNotFoundException {
		return new JavaInstance(LispJavaUtils.newInstance(klass, lst));
	}
	
	//
	/*package*/ Datum invokeMethod(
			String name, List<Datum> lst
			) throws ParameterNotFoundException {
		Object res;
		
		res = JavaUtils.invokeMethod(klass, null, name, lst);
		return LispUtils.toDatum(res);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<java-class>");
	}
	
}
