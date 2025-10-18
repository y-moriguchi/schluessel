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
package net.morilib.lisp;

import net.morilib.lisp.condition.LispCompoundCondition;
import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispSimpleCondition;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/15
 */
public class SRFI34 {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/15
	 */
	/*package*/ static class RaisedException extends LispException {

		//
		private static final long
		serialVersionUID = 2644902262840937292L;

		//
		/*package*/ Datum raised;

		//
		private static LispCondition makecond(String message) {
			LispCondition err;
			LispSimpleCondition msg;

			err = LispMessage.getConditionByKey("err.srfi34.raised");
			msg = LispSimpleCondition.newInstance("&message");
			msg.setField("message", new LispString(message));
			return new LispCompoundCondition(err, msg);
		}

		/**
		 * @param code
		 * @param message
		 */
		public RaisedException(String message, Datum obj) {
			super("err.srfi34.raised", message, makecond(message));
			raised = obj;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/15
	 */
	public static class WithExceptionHandler extends Subr {

		/*
		 * (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			throw new RuntimeException();
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		/*package*/ ClosureClass createClosureClass(Environment env) {
			CompiledCode.Builder bld = new CompiledCode.Builder();
			ClosureClass cl1 = new ClosureClass();
			Cons c1 = new Cons();
			Cons c2 = new Cons();
			int lbl3 = bld.allocLabel();

			bld.addReferSymbol(Symbol.getSymbol("handler"));
			bld.addEnterExceptionHandler(lbl3);
			bld.addReferSymbol(Symbol.getSymbol("thunk"));
			bld.addBeginList();
			bld.addEndList();
			bld.addCall();
			bld.setCurrentAddressToLabel(lbl3);
			bld.addLeaveExceptionHandler();
			bld.addReturnOp();

			c1.setCar(Symbol.getSymbol("handler"));
			c1.setCdr(c2);
			c2.setCar(Symbol.getSymbol("thunk"));
			cl1.setParameterList(c1);
			cl1.setCode(bld.getCodeRef());
			return cl1;
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/15
	 */
	public static class Raise extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			throw new RaisedException(
					mesg.get("err.srfi34.raised"), c1a);
		}

	}

}
