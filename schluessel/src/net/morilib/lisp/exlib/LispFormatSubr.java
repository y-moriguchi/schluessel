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
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.OutputPort;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.format.DatumFormatArguments;
import net.morilib.lisp.format.FormatArgumentsException;
import net.morilib.lisp.format.FormatParseException;
import net.morilib.lisp.format.LispFormat;
import net.morilib.lisp.format.LispFormatException;

public class LispFormatSubr extends Subr {
	
	private Cons checkGetCdr(Cons c1, LispMessage mesg) {
		if(c1.getCdr() instanceof Cons) {
			return (Cons)c1.getCdr();
		} else {
			throw mesg.getError(
					"err.parameter.insufficient", "format");
		}
	}
	
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		OutputPort outp = null;
		String formatted;
		
		if(body instanceof Cons) {
			Cons c1 = (Cons)body;
			
			if(c1.getCar() instanceof OutputPort) {
				outp = (OutputPort)c1.getCar();
				c1 = checkGetCdr(c1, mesg);
			} else if(LispBoolean.TRUE.equals(c1.getCar())) {
				outp = OutputPort.getStandard(mesg);
				c1 = checkGetCdr(c1, mesg);
			} else if(LispBoolean.FALSE.equals(c1.getCar())) {
				outp = null;
				c1 = checkGetCdr(c1, mesg);
			}
			
			if(c1.getCar().isTypeString()) {
				try {
					String fmt = c1.getCar().getString();
					LispString res;
					
					formatted = LispFormat.getInstance().format(
							fmt,
							new DatumFormatArguments(c1.getCdr()));
					res = new LispString(formatted);
					
					if(outp == null) {
						return res;
					} else {
						outp.display(res);
						return Undef.UNDEF;
					}
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
