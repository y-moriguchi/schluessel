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
package net.morilib.lisp.sos;

import java.util.List;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Subr;

public class ClassPrecedenceList extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator d = new ConsIterator(body);
		Datum d0;
		List<LispType> lst;
		ConsListBuilder bld = new ConsListBuilder();
		
		if(!d.hasNext()) {
			throw mesg.getError("err.argument");
		}
		
		d0 = d.next();
		if(!(d0 instanceof LispClass)) {
			throw mesg.getError("err.require.class");
		}
		if(d.getTerminal() != Nil.NIL) {
			throw mesg.getError("err.argument");
		}
		
		lst = ((LispClass)d0).getObjectType().getCPL();
		for(LispType t : lst) {
			bld.append(SOS.getInstance().getLispClass(t));
		}
		return bld.get();
	}

}
