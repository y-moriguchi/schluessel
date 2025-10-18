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
package net.morilib.lisp.collection;

import java.util.List;
import java.util.ListIterator;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.Subr;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.lisp.subr.UnaryArgs;
import net.morilib.util.Iterators;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/04
 */
public interface LispEnumeration extends Iterable<Datum> {

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class EnumerationFoldLeft extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public EnumerationFoldLeft(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum c1a = SubrUtils.nextIf(itr, mesg, body);
			Datum c2a = SubrUtils.nextIf(itr, mesg, body);
			Datum c3a = itr.rest();

			checkType(c1a, mesg);
			if(!(c1a instanceof LispEnumeration)) {
				throw mesg.getError(
						"err.srfi44.require.enumeration", c1a);
			} else {
				Datum[] r = LispUtils.toArray(c3a, mesg);

				for(Datum d : (LispEnumeration)c1a) {
					for(int i = 0; i < r.length; i++) {
						r[i] = Scheme.callva(c2a, env, mesg, d, r[i]);
					}
				}
				return MultiValues.newValues(r);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class EnumerationFoldRight extends Subr {

		//
		private Class<?> tocheck;
		private String   errcd;

		/**
		 * 
		 * @param cls
		 */
		public EnumerationFoldRight(Class<?> cls, String err) {
			tocheck = cls;
			errcd   = err;
		}

		//
		private void checkType(Datum c1a, LispMessage mesg) {
			SubrUtils.checkType(c1a, tocheck, mesg, errcd);
		}

		/* (non-Javadoc)
		 * @see net.morilib.lisp.Subr#eval(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			ConsIterator itr = new ConsIterator(body);
			Datum c1a = SubrUtils.nextIf(itr, mesg, body);
			Datum c2a = SubrUtils.nextIf(itr, mesg, body);
			Datum c3a = itr.rest();

			checkType(c1a, mesg);
			if(!(c1a instanceof LispEnumeration)) {
				throw mesg.getError(
						"err.srfi44.require.enumeration", c1a);
			} else {
				Datum[] r = LispUtils.toArray(c3a, mesg);
				List<Datum> l = Iterators.toList(
						((LispEnumeration)c1a).iterator());
				ListIterator<Datum> i2 = l.listIterator(l.size());

				while(i2.hasPrevious()) {
					Datum x = i2.previous();

					for(int i = 0; i < r.length; i++) {
						r[i] = Scheme.callva(c2a, env, mesg, x, r[i]);
					}
				}
				return MultiValues.newValues(r);
			}
		}

	}

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2011/05/04
	 */
	public static class IsEnumeration extends UnaryArgs {

		/* (non-Javadoc)
		 * @see net.morilib.lisp.subr.UnaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
		 */
		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			return LispBoolean.getInstance(
					c1a instanceof LispEnumeration);
		}

	}

}
