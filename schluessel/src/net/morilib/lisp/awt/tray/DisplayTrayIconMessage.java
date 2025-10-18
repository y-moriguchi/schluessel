/*
 * Copyright 2009-2010 Yuichiro Moriguchi
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
package net.morilib.lisp.awt.tray;

import java.awt.TrayIcon;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/01
 */
public class DisplayTrayIconMessage extends Subr {

	//
	private static final Symbol ERROR   = Symbol.getSymbol("error");
	private static final Symbol INFO    = Symbol.getSymbol("info");
	private static final Symbol NONE    = Symbol.getSymbol("none");
	private static final Symbol WARNING = Symbol.getSymbol("warning");

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum  io = SubrUtils.nextIf(itr, mesg, body);
		String tt = SubrUtils.nextString(itr, mesg, body);
		String mg = SubrUtils.nextString(itr, mesg, body);
		Datum  zz = Iterators.nextIf(itr, LispBoolean.FALSE);
		TrayIcon ti;

		if(!(io instanceof LispTrayIcon)) {
			throw mesg.getError("err.awt.require.trayicon", io);
		}

		ti = ((LispTrayIcon)io).icon;
		if(zz.equals(ERROR)) {
			ti.displayMessage(tt, mg, TrayIcon.MessageType.ERROR);
		} else if(zz.equals(NONE)) {
			ti.displayMessage(tt, mg, TrayIcon.MessageType.NONE);
		} else if(zz.equals(WARNING)) {
			ti.displayMessage(tt, mg, TrayIcon.MessageType.WARNING);
		} else if(zz.equals(INFO)) {
			ti.displayMessage(tt, mg, TrayIcon.MessageType.INFO);
		} else {
			ti.displayMessage(tt, mg, TrayIcon.MessageType.INFO);
		}
		return Undef.UNDEF;
	}

}
