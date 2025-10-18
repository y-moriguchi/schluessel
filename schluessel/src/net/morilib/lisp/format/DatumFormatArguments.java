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
package net.morilib.lisp.format;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;
import net.morilib.lisp.LispUtils;

public class DatumFormatArguments implements FormatArguments {
	
	//
	private Datum lst;
	
	
	public DatumFormatArguments(Datum lst) {
		this.lst = lst;
	}
	
	
	public char shiftChar() throws LispFormatException {
		Datum dtm = (Datum)shift();
		
		if(dtm instanceof LispCharacter) {
			return dtm.getCharacter();
		} else {
			throw new FormatArgumentsException();
		}
	}

	
	public int shiftInt() throws LispFormatException {
		Datum dtm = (Datum)shift();
		
		if(dtm instanceof LispInteger) {
			return dtm.getInt();
		} else {
			throw new FormatArgumentsException();
		}
	}


	public double shiftDouble() throws LispFormatException {
		Datum dtm = (Datum)shift();
		
		if(dtm instanceof LispReal) {
			return dtm.getRealDouble();
		} else {
			throw new FormatArgumentsException();
		}
	}


	public String shiftString() throws LispFormatException {
		Datum dtm = (Datum)shift();
		
		return LispUtils.print(dtm);
	}


	public String shiftStringWrite() throws LispFormatException {
		Datum dtm = (Datum)shift();
		
		return LispUtils.getResult(dtm);
	}


	public Object shift() throws LispFormatException {
		Datum res;
		
		if(lst instanceof Cons) {
			Cons c = (Cons)lst;
			
			res = c.getCar();
			lst = c.getCdr();
		} else {
			throw new FormatArgumentsException();
		}
		return res;
	}

}
