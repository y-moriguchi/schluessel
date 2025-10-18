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
package net.morilib.lisp.swing.event;

import java.awt.event.ActionEvent;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispString;
import net.morilib.lisp.datetime.LispDate;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/27
 */
public class LispActionEvent extends Datum {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class GetActionCommand extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispActionEvent) {
				LispActionEvent e = (LispActionEvent)c1a;

				return new LispString(e.event.getActionCommand());
			} else {
				throw mesg.getError(
						"err.swing.require.event.action", c1a);
			}
		}

	}

//	/**
//	 * 
//	 *
//	 *
//	 * @author MORIGUCHI, Yuichiro 2011/02/27
//	 */
//	public static class GetModifiers extends UnaryArgs {
//
//		/* (non-Javadoc)
//		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
//		 */
//		@Override
//		protected Datum execute(
//				Datum c1a, Environment env, LispMessage mesg) {
//			if(c1a instanceof LispActionEvent) {
//				LispActionEvent e = (LispActionEvent)c1a;
//
//				return new LispString(e.event.getActionCommand());
//			} else {
//				throw mesg.getError(
//						"err.swing.require.event.action", c1a);
//			}
//		}
//
//	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class GetWhen extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispActionEvent) {
				LispActionEvent e = (LispActionEvent)c1a;

				return new LispDate(e.event.getWhen());
			} else {
				throw mesg.getError(
						"err.swing.require.event.action", c1a);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/02/27
	 */
	public static class GetParamString extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof LispActionEvent) {
				LispActionEvent e = (LispActionEvent)c1a;

				return new LispString(e.event.paramString());
			} else {
				throw mesg.getError(
						"err.swing.require.event.action", c1a);
			}
		}

	}

	//
	private ActionEvent event;

	/**
	 * 
	 * @param e
	 */
	public LispActionEvent(ActionEvent e) {
		this.event = e;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<action-event>");
	}

}

