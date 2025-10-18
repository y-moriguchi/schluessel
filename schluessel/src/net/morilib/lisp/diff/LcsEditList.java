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
package net.morilib.lisp.diff;

import java.util.ArrayList;
import java.util.List;

import net.morilib.diff.Change;
import net.morilib.diff.Diff;
import net.morilib.lang.EqualPredicate;
import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.ConsListBuilder;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.subr.SubrUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/11
 */
public class LcsEditList extends Subr {

	//
	static final EqualPredicate DEF_EQ = new EqualPredicate() {

		public boolean isEqual(Object a, Object b) {
			return a != null ? a.equals(b) : b == null;
		}

	};

	//
	private static final Symbol PL = Symbol.getSymbol("+");
	private static final Symbol MI = Symbol.getSymbol("-");

	//
	private List<Object> tolist(Datum a) {
		List<Object> r = new ArrayList<Object>();
		ConsIterator i = new ConsIterator(a);

		while(i.hasNext()) {
			r.add(i.next());
		}
		return r;
	}

	//
	private Datum changetolist(Change<Object> c) {
		ConsListBuilder b = new ConsListBuilder();

		switch(c.getOperation()) {
		case Change.INSERTED:
			for(int i = 0; i < c.getAfter().size(); i++) {
				b.append(LispUtils.list(
						PL,
						LispInteger.valueOf(
								i + c.getBeginIndexA() - 1),
						c.getAfter().get(i)));
			}
			return b.get();
		case Change.DELETED:
			for(int i = 0; i < c.getBefore().size(); i++) {
				b.append(LispUtils.list(
						MI,
						LispInteger.valueOf(
								i + c.getBeginIndexA() - 1),
						c.getBefore().get(i)));
			}
			return b.get();
		case Change.CHANGED:
			for(int i = 0; i < c.getBefore().size(); i++) {
				b.append(LispUtils.list(
						MI,
						LispInteger.valueOf(
								i + c.getBeginIndexA() - 1),
						c.getBefore().get(i)));
			}
			for(int i = 0; i < c.getAfter().size(); i++) {
				b.append(LispUtils.list(
						PL,
						LispInteger.valueOf(
								i + c.getBeginIndexB() - 1),
						c.getAfter().get(i)));
			}
			return b.get();
		default:  throw new RuntimeException();
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body,
			final Environment env, final LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum a = SubrUtils.nextIf(itr, mesg, body);
		Datum b = SubrUtils.nextIf(itr, mesg, body);
		final Procedure p = SubrUtils.nextProcedureOptional(itr, mesg);
		List<Object> al, bl;
		List<Change<Object>> r;
		ConsListBuilder bld = new ConsListBuilder();
		EqualPredicate eq;

		SubrUtils.checkTerminated(itr, body, mesg);
		al = tolist(a);
		bl = tolist(b);

		if(p == null) {
			eq = DEF_EQ;
		} else {
			eq = new EqualPredicate() {

				public boolean isEqual(Object a, Object b) {
					return Scheme.callva(
							p, env, mesg, (Datum)a, (Datum)b).isTrue();
				}

			};
		}

		r  = Diff.diff(al, bl, eq);
		for(Change<Object> c : r) {
			bld.append(changetolist(c));
		}
		return bld.get();
	}

}
