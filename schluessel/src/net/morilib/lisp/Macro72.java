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
 * @author MORIGUCHI, Yuichiro 2012/03/03
 */
public final class Macro72 extends Datum {

	//
	private Closure mclosure;

	//
	/*package*/ Macro72(Closure cls) {
		if(cls == null) {
			throw new NullPointerException();
		}
		this.mclosure = cls;
	}

	//
	/*package*/ Closure getClosure() {
		return mclosure;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<macro>");
	}

}

