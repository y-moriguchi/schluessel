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
package net.morilib.lisp.r6rs.record;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.UnaryArgs;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/09/13
 */
public class RecordConstructor extends UnaryArgs {

	//
	private static void callProtocol(
			RecordTypeDescriptor rtd,
			RecordConstructorDescriptor rcd,
			R6RSRecord r,
			Datum body,
			Environment env,
			LispMessage mesg) {
		Prtcl prt = new Prtcl(rcd, rcd.getRtd(), r);
		Procedure pc;
		Datum p;

		pc = (rcd.getProtocol() != null) ?
				rcd.getProtocol() : new DefaultProtocol(rtd);
		p = Scheme.callva(pc, env, mesg, prt);
		if(p instanceof Procedure) {
			Scheme.call(p, env, mesg, body);
		} else {
			throw mesg.getError("err.require.procedure", p);
		}
	}

	//
	private static class Prtcl extends Subr {

		//
		private RecordConstructorDescriptor rcd;
		private RecordTypeDescriptor rtd;
		private R6RSRecord record;

		//
		private Prtcl(RecordConstructorDescriptor rcd,
				RecordTypeDescriptor rtd,
				R6RSRecord record) {
			super("record constructor protocol");
			this.rcd    = rcd;
			this.rtd    = rtd;
			this.record = record;
		}

		//
		private Datum callr(Datum body, LispMessage mesg) {
			Datum[] d = LispUtils.consToArray(body, mesg);

			try {
				rtd.initialize(record, d);
				return (Datum)record;
			} catch (R6RSRecordInitilizeException e) {
				throw mesg.getError(
						"err.r6rsrecord.initialize.record");
			}
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			RecordTypeDescriptor rtd2 = rtd.getParent();
			RecordConstructorDescriptor rcd2 = rcd.getParent();

			if(rtd2 == null) {
				return callr(body, mesg);
			} else {
				callProtocol(rtd2, rcd2, record, body, env, mesg);
				return new Subr("protocol") {
	
					@Override
					public Datum eval(Datum body, Environment env,
							LispMessage mesg) {
						return callr(body, mesg);
					}
	
				};
			}
		}

	}

	//
	private static class Constr extends Subr {

		//
		private RecordConstructorDescriptor rcd;

		//
		private Constr(RecordConstructorDescriptor rcd) {
			super("record constructor " + rcd.getRtd().getId());
			this.rcd = rcd;
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(Datum body, Environment env,
				LispMessage mesg) {
			R6RSRecord r = rcd.newInstance();

			callProtocol(rcd.getRtd(), rcd, r, body, env, mesg);
			return (Datum)r;
		}

	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	protected Datum execute(Datum c1a, Environment env,
			LispMessage mesg) {
		if(c1a instanceof RecordConstructorDescriptor) {
			RecordConstructorDescriptor rcd;

			rcd = (RecordConstructorDescriptor)c1a;
			return new Constr(rcd);
		} else {
			throw mesg.getError("err.r6rsrecord.require.rcd", c1a);
		}
	}

}
