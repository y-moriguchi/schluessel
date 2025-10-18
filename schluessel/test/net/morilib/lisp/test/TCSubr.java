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
package net.morilib.lisp.test;

import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.MultiValues;
import net.morilib.lisp.ReadException;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.LispException;

public abstract class TCSubr extends TCLisp {

	protected static void lperr(Scheme l, String v) {
		try {
			eq(l.input(v), F);
			fail();
		} catch(LispException e) {}
	}

	protected static void rderr(Scheme l, String v) {
		try {
			eq(l.input(v), F);
			fail();
		} catch(ReadException e) {}
	}

	protected static void eq(Scheme l, String v, Datum d) {
		eq(l.input(v), d);
	}

	protected static void eqv(Scheme l, String v, Datum d) {
		eqv(l.input(v), d);
	}

	protected static void equal(Scheme l, String v, Datum d) {
		equal(l.input(v), d);
	}

	protected static void equal(Scheme l, String v, Datum d, Datum e) {
		Datum res = l.input(v);

		if(res instanceof MultiValues) {
			List<Datum> lst = ((MultiValues)res).getValues();

			equal(lst.get(0), d);
			equal(lst.get(1), e);
		} else {
			fail();
		}
	}

	protected static void equal(
			Scheme l, String v, Datum d, Datum e, Datum f) {
		Datum res = l.input(v);

		if(res instanceof MultiValues) {
			List<Datum> lst = ((MultiValues)res).getValues();

			equal(lst.get(0), d);
			equal(lst.get(1), e);
			equal(lst.get(2), f);
		} else {
			fail();
		}
	}

	protected static void equal(
			Scheme l, String v, Datum d, Datum e, Datum f, Datum g) {
		Datum res = l.input(v);

		if(res instanceof MultiValues) {
			List<Datum> lst = ((MultiValues)res).getValues();

			equal(lst.get(0), d);
			equal(lst.get(1), e);
			equal(lst.get(2), f);
			equal(lst.get(3), g);
		} else {
			fail();
		}
	}

	protected static void equal(
			Scheme l, String v,
			Datum d, Datum e, Datum f, Datum g, Datum h) {
		Datum res = l.input(v);

		if(res instanceof MultiValues) {
			List<Datum> lst = ((MultiValues)res).getValues();

			equal(lst.get(0), d);
			equal(lst.get(1), e);
			equal(lst.get(2), f);
			equal(lst.get(3), g);
			equal(lst.get(4), h);
		} else {
			fail();
		}
	}

	protected static void equax(Scheme l, String v, Datum d) {
		equal(l, v, d);
		equal(l, "x", d);
	}

	protected static void isNaN(Scheme l, String v) {
		isNaN(l.input(v));
	}

	protected static void eqc(Scheme l, String exp, char c) {
		eqv(l, exp, chr(c));
	}

	protected static void eqi(Scheme l, String exp, int c) {
		eqv(l, exp, newZ(c));
	}

	protected static void eqi(Scheme l, String exp, int c, int d) {
		equal(l, exp, newZ(c), newZ(d));
	}

	protected static void eql(Scheme l, String exp, long c) {
		eqv(l, exp, newZ(c));
	}

	protected static void eql(Scheme l, String exp, long c, long d) {
		equal(l, exp, newZ(c), newZ(d));
	}

	protected static void eqr(Scheme l, String exp, double c) {
		eqv(l, exp, newR(c));
	}

	protected static void eqr(Scheme l, String exp, double c, double d) {
		equal(l, exp, newR(c), newR(d));
	}

	protected static void eqs(Scheme l, String exp, String s) {
		eq(l.input(exp).getString(), s);
	}

}
