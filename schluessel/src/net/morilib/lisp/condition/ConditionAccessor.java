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
package net.morilib.lisp.condition;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/04
 */
public class ConditionAccessor extends BinaryArgs {

	//
	private static class Proc extends UnaryArgs {

		//
		private LispConditionType type;
		private Procedure p;

		//
		private Proc(LispConditionType type, Procedure p) {
			super("condition-accessor of " + type.getId());
			this.type = type;
			this.p    = p;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(Datum c1a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof LispCondition) {
				List<LispSimpleCondition> l;

				l = ((LispCondition)c1a).getConditions();
				for(LispSimpleCondition c : l) {
					if(c.hasType(type)) {
						return Scheme.callva(p, env, mesg, c);
					}
				}
				return LispBoolean.FALSE;
			} else {
				throw mesg.getError(
						"err.condition.require.condition", c1a);
			}
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Datum c2a, Environment env,
			LispMessage mesg) {
		if(!(c1a instanceof LispConditionType)) {
			throw mesg.getError(
					"err.condition.require.conditiontype", c1a);
		} else if(!(c2a instanceof Procedure)) {
			throw mesg.getError("err.require.procedure", c2a);
		} else {
			return new Proc((LispConditionType)c1a, (Procedure)c2a);
		}
	}

}
