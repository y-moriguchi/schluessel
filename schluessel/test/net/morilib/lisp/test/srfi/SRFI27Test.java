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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/25
 */
public class SRFI27Test extends TCSubr {

	public void testRandomInteger() {
		Scheme l = Scheme.newInstance();
		int r = l.callExactInt("random-integer", 100);

		System.out.println(r);
		ok(r >= 0 && r < 100);
	}

	public void testRandomReal() {
		Scheme l = Scheme.newInstance();
		double r = l.callDouble("random-real");

		System.out.println(r);
		ok(r > 0.0 && r < 1.0);
	}

	public void testIsRandomSource() {
		Scheme l = Scheme.newInstance();

		equal(l, "(random-source? (make-random-source))", T);
		equal(l, "(random-source? 1)", F);
	}

	public void testRandomSourceState() {
		Scheme l = Scheme.newInstance();
		int r, s;

		l.exec("(define x  (make-random-source))");
		l.exec("(define p  (random-source-make-integers x))");
		l.exec("(define st (random-source-state-ref x))");
		r = l.callExactInt("p", 100000);
		l.exec("(random-source-state-set! x st)");
		s = l.callExactInt("p", 100000);
		System.out.println(r + ":" + s);
		ok(r == s);
	}

	public void testRandomSourcePseudoRandomize() {
		Scheme l = Scheme.newInstance();
		int r, s;

		l.exec("(define x  (make-random-source))");
		l.exec("(define p  (random-source-make-integers x))");
		l.exec("(random-source-pseudo-randomize! x 765 876)");
		r = l.callExactInt("p", 100000);
		l.exec("(random-source-pseudo-randomize! x 765 876)");
		s = l.callExactInt("p", 100000);
		System.out.println(r + ":" + s);
		ok(r == s);
	}

	public void testRandomSourceMakeIntegers() {
		Scheme l = Scheme.newInstance();
		int r;

		l.exec("(define x (make-random-source))");
		l.exec("(define p (random-source-make-integers x))");
		r = l.callExactInt("p", 100);
		System.out.println(r);
		ok(r >= 0 && r < 100);
	}

	public void testRandomSourceMakeReals1() {
		Scheme l = Scheme.newInstance();
		double r;

		l.exec("(define x (make-random-source))");
		l.exec("(define p (random-source-make-reals x))");
		r = l.callDouble("p");
		System.out.println(r);
		ok(r > 0.0 && r < 1.0);
	}

	public void testRandomSourceMakeReals2() {
		Scheme l = Scheme.newInstance();
		double r;

		l.exec("(define x (make-random-source))");
		l.exec("(define p (random-source-make-reals x 0.5))");
		r = l.callDouble("p");
		System.out.println(r);
		ok(r > 0.0 && r < 0.5);
	}

}
