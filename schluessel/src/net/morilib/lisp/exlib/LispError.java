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
package net.morilib.lisp.exlib;

import net.morilib.lisp.Cons;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.UserException;
import net.morilib.lisp.format.DatumFormatArguments;
import net.morilib.lisp.format.FormatArgumentsException;
import net.morilib.lisp.format.FormatParseException;
import net.morilib.lisp.format.LispFormat;
import net.morilib.lisp.format.LispFormatException;

public class LispError extends Subr {
	
	/*
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		StringBuilder b = new StringBuilder();
		List<Datum> lst = LispUtils.consToList(body, mesg);
		
		if(lst.size() < 1) {
			throw mesg.getError("err.argument", symbolName);
		}
		
		b.append(LispUtils.print(lst.get(0)));
		for(int i = 1; i < lst.size(); i++) {
			b.append((i > 1) ? " " : ":");
			b.append(LispUtils.print(lst.get(i)));
		}
		throw new UserException(b.toString());
	}
	*/
	
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		String formatted;
		
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			
			if(c1.getCar().isTypeString()) {
				try {
					String fmt = c1.getCar().getString();
					
					formatted = LispFormat.getInstance().format(
							fmt,
							new DatumFormatArguments(c1.getCdr()));
					throw new UserException(formatted);
				} catch (FormatArgumentsException e) {
					throw mesg.getError(
							"err.srfi28.arguments.insufficient");
				} catch (FormatParseException e) {
					throw mesg.getError(
							"err.srfi28.format.syntax");
				} catch (LispFormatException e) {
					throw mesg.getError("err.srfi28.format");
				}
			}
		}
		throw mesg.getError(
				"err.parameter.insufficient", "format");
	}

}
