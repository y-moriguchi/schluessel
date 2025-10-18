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
package net.morilib.lisp.swing;

import javax.swing.UIManager;
import javax.swing.UnsupportedLookAndFeelException;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Undef;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/07/23
 */
public class LookAndFeelSetS extends UnaryArgs {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		LispLookAndFeelInfo l;
		String s = SubrUtils.getSymbolName(c1a, mesg);

		if((l = LispLookAndFeelInfo.LnFS.get(s)) == null) {
			throw mesg.getError("err.swing.lookandfeel.notfound", s);
		} else {
			try {
				UIManager.setLookAndFeel(l.getClassName());
				return Undef.UNDEF;
			} catch (ClassNotFoundException e) {
				throw mesg.getError("err.swing.lookandfeel.error");
			} catch (InstantiationException e) {
				throw mesg.getError("err.swing.lookandfeel.error");
			} catch (IllegalAccessException e) {
				throw mesg.getError("err.swing.lookandfeel.error");
			} catch (UnsupportedLookAndFeelException e) {
				throw mesg.getError("err.swing.lookandfeel.error");
			}
		}
	}

}
