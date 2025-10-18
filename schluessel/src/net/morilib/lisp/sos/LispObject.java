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

import java.util.HashMap;
import java.util.Map;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;

public final class LispObject extends SlotDatum {
	
	//
	private LispClass klass;
	private Map<Symbol, Datum> slotval;
	private LispClass nwklass = null;
	private Map<Symbol, Datum> newvals = null;
	
	
	/*package*/ LispObject(LispClass cls, Map<Symbol, Datum> initval) {
		super(null);
		klass   = cls;
		slotval = initval;
	}
	
	
	/*package*/ LispObject(LispObject obj) {
		super(obj.getName());
		klass = obj.klass;
		slotval = new HashMap<Symbol, Datum>(obj.slotval);
	}
	
	
	/*package*/ void reinstantiate() {
		if(nwklass != null) {
			slotval = nwklass.reinstantiate(slotval, newvals, klass);
			klass   = nwklass;
			nwklass = null;
			newvals = null;
		}
	}
	
	
	/*package*/ void copyFrom(LispObject obj) {
		slotval = klass.reinstantiate(obj.slotval, slotval, obj.klass);
	}
	
	
	public Datum getSlot(Symbol sym) {
		Datum v;
		
		if(nwklass != null) {
			reinstantiate();
		}
		
		v = slotval.get(sym);
		return (v == null) ? klass.getSlot0(sym) : v;
	}
	
	
	public boolean setSlot(Symbol sym, Datum val, LispMessage mesg) {
		if(nwklass != null) {
			reinstantiate();
		}
		
		if(slotval.containsKey(sym)) {
			slotval.put(sym, val);
			return true;
		} else {
			return klass.setSlot0(sym, val);
		}
	}
	
	
	/*package*/ void setNewClass(
			LispClass nwkls, Map<Symbol, Datum> vals) {
		nwklass = nwkls;
		newvals = vals;
	}
	
	
	/*package*/ LispClass getLispClass() {
		return klass;
	}
	
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.NamableDatum#display()
	 */
	@Override
	public String display() {
		return "#<" + klass.printName() + " " + printName() + ">";
	}
	
	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#getType()
	 */
	@Override
	public LispType getType() {
		return (nwklass == null) ?
				klass.getObjectType() : nwklass.getObjectType();
	}
	
}
