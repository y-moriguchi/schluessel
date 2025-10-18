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

import java.util.List;

import net.morilib.lisp.condition.LispCondition;
import net.morilib.lisp.condition.LispConditionType;
import net.morilib.lisp.condition.LispSimpleCondition;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.SubrPredicate;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/01/15
 */
public class ExceptionObject extends Datum implements LispCondition {

	//
	private LispException exception;

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/16
	 */
	public abstract static class
	IsExceptionType extends SubrPredicate {

		/**
		 * 
		 * @param errorCode
		 * @return
		 */
		protected abstract boolean validate(String errorCode);

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.SubrPredicate#test(net.morilib.lisp.Datum)
		 */
		@Override
		protected boolean test(Datum d) {
			if(d instanceof ExceptionObject) {
				return validate(
						((ExceptionObject)d).exception.getErrorCode());
			} else {
				return false;
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/15
	 */
	public static class IsException extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof ExceptionObject);
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/01/15
	 */
	public static class IsExceptionCode extends BinaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(!(c1a instanceof ExceptionObject)) {
				throw mesg.getError("err.require.exception", c1a);
			} else if(!(c2a instanceof Symbol)) {
				throw mesg.getError("err.require.symbol", c2a);
			}

			ExceptionObject e = (ExceptionObject)c1a;
			String s = ((Symbol)c2a).getName();
			return LispBoolean.getInstance(
					e.exception.getErrorCode().equals(s));
		}

	}

	/**
	 * 
	 * @param exception
	 */
	/*package*/ ExceptionObject(LispException exception) {
		this.exception = exception;
	}

	/**
	 * @return the exception
	 */
	public LispException getException() {
		return exception;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#getSlot(net.morilib.lisp.Symbol)
	 */
	public Datum getSlot(Symbol sym) {
		return exception.getCondition().getSlot(sym);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#setSlot(net.morilib.lisp.Symbol, net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	public boolean setSlot(Symbol sym, Datum val, LispMessage mesg) {
		return exception.getCondition().setSlot(sym, val, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getField(java.lang.String)
	 */
	public Datum getField(String v) {
		return exception.getCondition().getField(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#setField(java.lang.String, net.morilib.lisp.Datum)
	 */
	public boolean setField(String v, Datum val) {
		return exception.getCondition().setField(v, val);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#hasType(net.morilib.lisp.condition.LispConditionType)
	 */
	public boolean hasType(LispConditionType type) {
		return exception.getCondition().hasType(type);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#getConditions()
	 */
	public List<LispSimpleCondition> getConditions() {
		return exception.getCondition().getConditions();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#extractCondition(net.morilib.lisp.condition.LispConditionType)
	 */
	public LispSimpleCondition extractCondition(LispConditionType t) {
		return exception.getCondition().extractCondition(t);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.condition.LispCondition#describeShort()
	 */
	public String describeShort() {
		return exception.getCondition().describeShort();
	}

}
