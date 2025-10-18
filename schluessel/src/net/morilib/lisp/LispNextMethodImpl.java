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

import java.util.Iterator;

import net.morilib.lisp.sos.LispTypeList;

/*package*/ final class LispNextMethodImpl extends LispNextMethod {
	
	//
	private LispGeneric method;
	private Iterator<LispTypeList> iter;
	private Closure clo;
	private Datum defparams;
	
	
	/*package*/ LispNextMethodImpl(
			LispGeneric mth,
			Iterator<LispTypeList> iter,
			Datum defp) {
		if(mth == null || iter == null || defp == null) {
			throw new NullPointerException();
		}
		this.method = mth;
		this.iter   = iter;
		this.defparams   = defp;
		if(iter.hasNext()) {
			clo = mth.get(iter.next());
		}
	}
	
	
	/*package*/ Datum getDefaultParams() {
		return defparams;
	}
	
	
	/*package*/ LispGeneric getMethod() {
		return method;
	}
	
	
	public Closure get() {
		return clo;
	}
	
	
	public LispNextMethodImpl getNextMethod() {
		return new LispNextMethodImpl(method, iter, defparams);
	}
	
}
