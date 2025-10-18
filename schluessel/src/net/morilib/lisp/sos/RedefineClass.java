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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.SymbolName;
import net.morilib.lisp.util.ConsIterable;

public class RedefineClass extends Subr {

	private List<Symbol> glist(Datum lx, LispMessage mesg) {
		List<Symbol> res = new ArrayList<Symbol>();
		
		for(Datum d : new ConsIterable(lx, mesg)) {
			if(d instanceof SymbolName) {
				res.add(((SymbolName)d).getSymbol());
			} else {
				throw mesg.getError("err.require.symbol", d);
			}
		}
		return res;
	}
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
//		throw mesg.getError("err.notsupport");
		List<Datum> l0 = LispUtils.consToList(body, mesg);
		List<LispClass> l1 = new ArrayList<LispClass>();
		List<Symbol> l2, l3, l4;
		LispObject meta = null;
		LispClass r0;
		Map<Symbol, Datum> vals;
		
		if(l0.size() < 6 || l0.size() > 7) {
			throw mesg.getError("err.argument", body);
		}
		
		if(l0.get(0) instanceof LispClass) {
			r0 = (LispClass)l0.get(0);
		} else {
			throw mesg.getError("err.require.class", l0.get(0));
		}
		
		for(Datum d : new ConsIterable(l0.get(1), mesg)) {
			if(d instanceof LispClass) {
				l1.add((LispClass)d);
			} else {
				throw mesg.getError("err.require.class", d);
			}
		}
		l2 = glist(l0.get(2), mesg);
		l3 = glist(l0.get(3), mesg);
		l4 = glist(l0.get(4), mesg);
		
		if((vals = LispUtils.assocToMapSymbol(l0.get(5))) == null) {
			throw mesg.getError("err.require.assoc");
		}
		
		if(l0.size() >= 7) {
			Datum z = l0.get(6);
			
			if(z instanceof LispObject) {
				meta = (LispObject)z;
			} else {
				throw mesg.getError("err.require.class", z);
			}
		}
		
		try {
			return SOS.getInstance().redefineClass(
					r0, l1, l2, l3, l4, vals, meta);
		} catch (LispTypeException e) {
			String msg = e.getMessage();
			
			if(msg == null || msg.equals("")) {
				throw mesg.getError("err.inherit.contradict");
			} else {
				throw mesg.getError(msg);
			}
		}
	}

}
