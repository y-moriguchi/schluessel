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

import net.morilib.lisp.sos.LispClass;
import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.util.ConsIterable;

public class SubrInstantiateGeneric extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		Iterator<Datum> itr = new ConsIterable(body, mesg).iterator();
		Datum d1;
		LispType   tp;
		LispGeneric mth;
		
		if(!itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(!((d1 = itr.next()) instanceof LispClass)) {
			throw mesg.getError("err.require.class", d1);
		} else if(itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		}
		
		tp  = ((LispClass)d1).getObjectType();
		if(tp.getCPL().contains(LispType.GENERIC)) {
			mth = new LispGeneric();
			mth.setType(tp);
			return mth;
		} else {
			throw mesg.getError("err.class.inherit.generic", d1);
		}
	}

}
