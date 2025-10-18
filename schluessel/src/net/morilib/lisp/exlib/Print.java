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

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.OutputPort;
import net.morilib.lisp.subr.WriteBase;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/04/17
 */
public class Print extends WriteBase {

	//
	private static final Datum NEWLINE = new LispString("\n");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.WriteBase#action(net.morilib.lisp.OutputPort, net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected void action(OutputPort outp, Datum d, LispMessage mesg) {
		outp.display(d);
		outp.display(NEWLINE);
	}

}
