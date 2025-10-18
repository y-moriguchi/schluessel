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

import java.util.Iterator;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.SymbolName;
import net.morilib.lisp.Undef;
import net.morilib.lisp.util.ConsIterable;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class SlotSetS extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		Iterator<Datum> itr = new ConsIterable(body, mesg).iterator();
		Datum d1, d2, d3;

		if(!itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(!((d1 = itr.next()) instanceof ISlotDatum)) {
			throw mesg.getError("err.require.slot", d1);
		} else if(!itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		} else if(!((d2 = itr.next()) instanceof SymbolName)) {
			throw mesg.getError("err.require.symbol", d2);
		} else if(!itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		}
		d3 = itr.next();

		if(itr.hasNext()) {
			throw mesg.getError("err.argument", body);
		}

		if(!((ISlotDatum)d1).setSlot(
				((SymbolName)d2).getSymbol(), d3, mesg)) {
			throw mesg.getError("err.slot.undefined", d2);
		}
		return Undef.UNDEF;
	}

}
