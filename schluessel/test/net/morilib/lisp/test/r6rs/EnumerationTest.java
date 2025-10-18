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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/22
 */
public class EnumerationTest extends TCSubr {

	public void testEnumeration() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define l (make-enumeration '(iori ami mami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cl (enum-set-constructor l))");
		l.input("(define cr (enum-set-constructor r))");
		l.input("(define f (ce '(ami iori)))");
		l.input("(define i (enum-set-indexer e))");

		lperr(l,"(ce '(ritsuko ryo))");

		equal(l,"(enum-set-universe f)",
				lsym("ritsuko", "azusa", "iori", "ami"));
		equal(l,"(enum-set-universe e)",
				lsym("ritsuko", "azusa", "iori", "ami"));

		eqi  (l,"(i 'ritsuko)", 0);
		eqi  (l,"(i 'azusa)", 1);
		eqi  (l,"(i 'iori)", 2);
		eqi  (l,"(i 'ami)", 3);
		eq   (l,"(i 'ryo)", F);

		equal(l,"(enum-set->list f)", lsym("iori", "ami"));
		equal(l,"(enum-set->list e)",
				lsym("ritsuko", "azusa", "iori", "ami"));

		eq   (l,"(enum-set-member? 'ritsuko f)", F);
		eq   (l,"(enum-set-member? 'azusa f)", F);
		eq   (l,"(enum-set-member? 'iori f)", T);
		eq   (l,"(enum-set-member? 'ami f)", T);
		eq   (l,"(enum-set-member? 'ryo f)", F);
		eq   (l,"(enum-set-member? 'ritsuko e)", T);
		eq   (l,"(enum-set-member? 'azusa e)", T);
		eq   (l,"(enum-set-member? 'iori e)", T);
		eq   (l,"(enum-set-member? 'ami e)", T);
		eq   (l,"(enum-set-member? 'ryo e)", F);
	}

	public void testSubset() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define l (make-enumeration '(iori ami mami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cl (enum-set-constructor l))");
		l.input("(define cr (enum-set-constructor r))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eq   (l,"(enum-set-subset? a1 a1)", T);
		eq   (l,"(enum-set-subset? a1 a2)", T);
		eq   (l,"(enum-set-subset? a1 a3)", F);
		eq   (l,"(enum-set-subset? a1 a4)", F);
		eq   (l,"(enum-set-subset? a1 ph)", F);
		eq   (l,"(enum-set-subset? a1  e)", T);
		eq   (l,"(enum-set-subset? a2 a1)", F);
		eq   (l,"(enum-set-subset? a2 a2)", T);
		eq   (l,"(enum-set-subset? a2 a3)", F);
		eq   (l,"(enum-set-subset? a2 a4)", F);
		eq   (l,"(enum-set-subset? a2 ph)", F);
		eq   (l,"(enum-set-subset? a2  e)", T);
		eq   (l,"(enum-set-subset? a3 a1)", F);
		eq   (l,"(enum-set-subset? a3 a2)", F);
		eq   (l,"(enum-set-subset? a3 a3)", T);
		eq   (l,"(enum-set-subset? a3 a4)", F);
		eq   (l,"(enum-set-subset? a3 ph)", F);
		eq   (l,"(enum-set-subset? a3  e)", T);
		eq   (l,"(enum-set-subset? a4 a1)", F);
		eq   (l,"(enum-set-subset? a4 a2)", T);
		eq   (l,"(enum-set-subset? a4 a3)", T);
		eq   (l,"(enum-set-subset? a4 a4)", T);
		eq   (l,"(enum-set-subset? a4 ph)", F);
		eq   (l,"(enum-set-subset? a4  e)", T);
		eq   (l,"(enum-set-subset? ph a1)", T);
		eq   (l,"(enum-set-subset? ph a2)", T);
		eq   (l,"(enum-set-subset? ph a3)", T);
		eq   (l,"(enum-set-subset? ph a4)", T);
		eq   (l,"(enum-set-subset? ph ph)", T);
		eq   (l,"(enum-set-subset? ph  e)", T);
		eq   (l,"(enum-set-subset?  e a1)", F);
		eq   (l,"(enum-set-subset?  e a2)", F);
		eq   (l,"(enum-set-subset?  e a3)", F);
		eq   (l,"(enum-set-subset?  e a4)", F);
		eq   (l,"(enum-set-subset?  e ph)", F);
		eq   (l,"(enum-set-subset?  e  e)", T);

		l.input("(define b1 (ce '(ritsuko iori)))");
		l.input("(define b2 (ce '(iori)))");
		l.input("(define b3 (cr '(ritsuko iori)))");
		l.input("(define b4 (cr '(iori)))");
		eq   (l,"(enum-set-subset? b1 b1)", T);
		eq   (l,"(enum-set-subset? b1 b2)", F);
		eq   (l,"(enum-set-subset? b1 b3)", F);
		eq   (l,"(enum-set-subset? b1 b4)", F);
		eq   (l,"(enum-set-subset? b1  e)", T);
		eq   (l,"(enum-set-subset? b1  r)", F);
		eq   (l,"(enum-set-subset? b2 b1)", T);
		eq   (l,"(enum-set-subset? b2 b2)", T);
		eq   (l,"(enum-set-subset? b2 b3)", F);
		eq   (l,"(enum-set-subset? b2 b4)", F);
		eq   (l,"(enum-set-subset? b2  e)", T);
		eq   (l,"(enum-set-subset? b2  r)", F);
		eq   (l,"(enum-set-subset? b3 b1)", T);
		eq   (l,"(enum-set-subset? b3 b2)", F);
		eq   (l,"(enum-set-subset? b3 b3)", T);
		eq   (l,"(enum-set-subset? b3 b4)", F);
		eq   (l,"(enum-set-subset? b3  e)", T);
		eq   (l,"(enum-set-subset? b3  r)", T);
		eq   (l,"(enum-set-subset? b4 b1)", T);
		eq   (l,"(enum-set-subset? b4 b2)", T);
		eq   (l,"(enum-set-subset? b4 b3)", T);
		eq   (l,"(enum-set-subset? b4 b4)", T);
		eq   (l,"(enum-set-subset? b4  e)", T);
		eq   (l,"(enum-set-subset? b4  r)", T);
		eq   (l,"(enum-set-subset?  e b1)", F);
		eq   (l,"(enum-set-subset?  e b2)", F);
		eq   (l,"(enum-set-subset?  e b3)", F);
		eq   (l,"(enum-set-subset?  e b4)", F);
		eq   (l,"(enum-set-subset?  e  e)", T);
		eq   (l,"(enum-set-subset?  e  r)", F);
		eq   (l,"(enum-set-subset?  r b1)", T);
		eq   (l,"(enum-set-subset?  r b2)", F);
		eq   (l,"(enum-set-subset?  r b3)", T);
		eq   (l,"(enum-set-subset?  r b4)", F);
		eq   (l,"(enum-set-subset?  r  e)", T);
		eq   (l,"(enum-set-subset?  r  r)", T);

		l.input("(define c1 (ce '(iori ami)))");
		l.input("(define c2 (cl '(iori ami)))");
		eq   (l,"(enum-set-subset? c1 c2)", F);
		eq   (l,"(enum-set-subset? c2 c1)", F);
	}

	public void testSetEq() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define l (make-enumeration '(iori ami mami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cl (enum-set-constructor l))");
		l.input("(define cr (enum-set-constructor r))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eq   (l,"(enum-set=? a1 a1)", T);
		eq   (l,"(enum-set=? a1 a2)", F);
		eq   (l,"(enum-set=? a1 a3)", F);
		eq   (l,"(enum-set=? a1 a4)", F);
		eq   (l,"(enum-set=? a1 ph)", F);
		eq   (l,"(enum-set=? a1  e)", F);
		eq   (l,"(enum-set=? a2 a1)", F);
		eq   (l,"(enum-set=? a2 a2)", T);
		eq   (l,"(enum-set=? a2 a3)", F);
		eq   (l,"(enum-set=? a2 a4)", F);
		eq   (l,"(enum-set=? a2 ph)", F);
		eq   (l,"(enum-set=? a2  e)", F);
		eq   (l,"(enum-set=? a3 a1)", F);
		eq   (l,"(enum-set=? a3 a2)", F);
		eq   (l,"(enum-set=? a3 a3)", T);
		eq   (l,"(enum-set=? a3 a4)", F);
		eq   (l,"(enum-set=? a3 ph)", F);
		eq   (l,"(enum-set=? a3  e)", F);
		eq   (l,"(enum-set=? a4 a1)", F);
		eq   (l,"(enum-set=? a4 a2)", F);
		eq   (l,"(enum-set=? a4 a3)", F);
		eq   (l,"(enum-set=? a4 a4)", T);
		eq   (l,"(enum-set=? a4 ph)", F);
		eq   (l,"(enum-set=? a4  e)", F);
		eq   (l,"(enum-set=? ph a1)", F);
		eq   (l,"(enum-set=? ph a2)", F);
		eq   (l,"(enum-set=? ph a3)", F);
		eq   (l,"(enum-set=? ph a4)", F);
		eq   (l,"(enum-set=? ph ph)", T);
		eq   (l,"(enum-set=? ph  e)", F);
		eq   (l,"(enum-set=?  e a1)", F);
		eq   (l,"(enum-set=?  e a2)", F);
		eq   (l,"(enum-set=?  e a3)", F);
		eq   (l,"(enum-set=?  e a4)", F);
		eq   (l,"(enum-set=?  e ph)", F);
		eq   (l,"(enum-set=?  e  e)", T);

		l.input("(define b1 (ce '(ritsuko iori)))");
		l.input("(define b2 (ce '(iori)))");
		l.input("(define b3 (cr '(ritsuko iori)))");
		l.input("(define b4 (cr '(iori)))");
		eq   (l,"(enum-set=? b1 b1)", T);
		eq   (l,"(enum-set=? b1 b2)", F);
		eq   (l,"(enum-set=? b1 b3)", F);
		eq   (l,"(enum-set=? b1 b4)", F);
		eq   (l,"(enum-set=? b1  e)", F);
		eq   (l,"(enum-set=? b1  r)", F);
		eq   (l,"(enum-set=? b2 b1)", F);
		eq   (l,"(enum-set=? b2 b2)", T);
		eq   (l,"(enum-set=? b2 b3)", F);
		eq   (l,"(enum-set=? b2 b4)", F);
		eq   (l,"(enum-set=? b2  e)", F);
		eq   (l,"(enum-set=? b2  r)", F);
		eq   (l,"(enum-set=? b3 b1)", F);
		eq   (l,"(enum-set=? b3 b2)", F);
		eq   (l,"(enum-set=? b3 b3)", T);
		eq   (l,"(enum-set=? b3 b4)", F);
		eq   (l,"(enum-set=? b3  e)", F);
		eq   (l,"(enum-set=? b3  r)", T);
		eq   (l,"(enum-set=? b4 b1)", F);
		eq   (l,"(enum-set=? b4 b2)", F);
		eq   (l,"(enum-set=? b4 b3)", F);
		eq   (l,"(enum-set=? b4 b4)", T);
		eq   (l,"(enum-set=? b4  e)", F);
		eq   (l,"(enum-set=? b4  r)", F);
		eq   (l,"(enum-set=?  e b1)", F);
		eq   (l,"(enum-set=?  e b2)", F);
		eq   (l,"(enum-set=?  e b3)", F);
		eq   (l,"(enum-set=?  e b4)", F);
		eq   (l,"(enum-set=?  e  e)", T);
		eq   (l,"(enum-set=?  e  r)", F);
		eq   (l,"(enum-set=?  r b1)", F);
		eq   (l,"(enum-set=?  r b2)", F);
		eq   (l,"(enum-set=?  r b3)", T);
		eq   (l,"(enum-set=?  r b4)", F);
		eq   (l,"(enum-set=?  r  e)", F);
		eq   (l,"(enum-set=?  r  r)", T);

		l.input("(define c1 (ce '(iori ami)))");
		l.input("(define c2 (cl '(iori ami)))");
		eq   (l,"(enum-set=? c1 c2)", F);
		eq   (l,"(enum-set=? c2 c1)", F);
	}

	static void eqenu(Scheme l, String s, String... syms) {
		equal(l,"(enum-set->list " + s + ")", lsym(syms));
	}

	public void testUnion() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cr (enum-set-constructor r))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eqenu(l,"(enum-set-union a1 a1)", "iori", "ami");
		eqenu(l,"(enum-set-union a1 a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a1 a3)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a1 a4)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a1 ph)", "iori", "ami");
		eqenu(l,"(enum-set-union a1  e)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2 a1)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2 a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2 a3)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2 a4)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2 ph)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a2  e)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a3 a1)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a3 a2)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a3 a3)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-union a3 a4)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-union a3 ph)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-union a3  e)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a4 a1)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a4 a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union a4 a3)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-union a4 a4)", "azusa", "iori");
		eqenu(l,"(enum-set-union a4 ph)", "azusa", "iori");
		eqenu(l,"(enum-set-union a4  e)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union ph a1)", "iori", "ami");
		eqenu(l,"(enum-set-union ph a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union ph a3)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-union ph a4)", "azusa", "iori");
		eqenu(l,"(enum-set-union ph ph)");
		eqenu(l,"(enum-set-union ph  e)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e a1)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e a2)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e a3)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e a4)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e ph)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-union  e  e)", "ritsuko", "azusa", "iori", "ami");

		l.input("(define c1 (ce '(iori)))");
		l.input("(define c2 (cr '(iori)))");
		lperr(l,"(enum-set-union c1 c2)");
		lperr(l,"(enum-set-union c2 c1)");
	}

	public void testIntersection() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cr (enum-set-constructor r))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eqenu(l,"(enum-set-intersection a1 a1)", "iori", "ami");
		eqenu(l,"(enum-set-intersection a1 a2)", "iori", "ami");
		eqenu(l,"(enum-set-intersection a1 a3)", "iori");
		eqenu(l,"(enum-set-intersection a1 a4)", "iori");
		eqenu(l,"(enum-set-intersection a1 ph)");
		eqenu(l,"(enum-set-intersection a1  e)", "iori", "ami");
		eqenu(l,"(enum-set-intersection a2 a1)", "iori", "ami");
		eqenu(l,"(enum-set-intersection a2 a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-intersection a2 a3)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a2 a4)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a2 ph)");
		eqenu(l,"(enum-set-intersection a2  e)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-intersection a3 a1)", "iori");
		eqenu(l,"(enum-set-intersection a3 a2)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a3 a3)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a3 a4)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a3 ph)");
		eqenu(l,"(enum-set-intersection a3  e)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a4 a1)", "iori");
		eqenu(l,"(enum-set-intersection a4 a2)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a4 a3)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a4 a4)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection a4 ph)");
		eqenu(l,"(enum-set-intersection a4  e)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection ph a1)");
		eqenu(l,"(enum-set-intersection ph a2)");
		eqenu(l,"(enum-set-intersection ph a3)");
		eqenu(l,"(enum-set-intersection ph a4)");
		eqenu(l,"(enum-set-intersection ph ph)");
		eqenu(l,"(enum-set-intersection ph  e)");
		eqenu(l,"(enum-set-intersection  e a1)", "iori", "ami");
		eqenu(l,"(enum-set-intersection  e a2)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-intersection  e a3)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-intersection  e a4)", "azusa", "iori");
		eqenu(l,"(enum-set-intersection  e ph)");
		eqenu(l,"(enum-set-intersection  e  e)", "ritsuko", "azusa", "iori", "ami");

		l.input("(define c1 (ce '(iori)))");
		l.input("(define c2 (cr '(iori)))");
		lperr(l,"(enum-set-intersection c1 c2)");
		lperr(l,"(enum-set-intersection c2 c1)");
	}

	public void testDifference() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define r (make-enumeration '(iori ritsuko)))");
		l.input("(define ce (enum-set-constructor e))");
		l.input("(define cr (enum-set-constructor r))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eqenu(l,"(enum-set-difference a1 a1)");
		eqenu(l,"(enum-set-difference a1 a2)");
		eqenu(l,"(enum-set-difference a1 a3)", "ami");
		eqenu(l,"(enum-set-difference a1 a4)", "ami");
		eqenu(l,"(enum-set-difference a1 ph)", "iori", "ami");
		eqenu(l,"(enum-set-difference a1  e)");
		eqenu(l,"(enum-set-difference a2 a1)", "azusa");
		eqenu(l,"(enum-set-difference a2 a2)");
		eqenu(l,"(enum-set-difference a2 a3)", "ami");
		eqenu(l,"(enum-set-difference a2 a4)", "ami");
		eqenu(l,"(enum-set-difference a2 ph)", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-difference a2  e)");
		eqenu(l,"(enum-set-difference a3 a1)", "ritsuko", "azusa");
		eqenu(l,"(enum-set-difference a3 a2)", "ritsuko");
		eqenu(l,"(enum-set-difference a3 a3)");
		eqenu(l,"(enum-set-difference a3 a4)", "ritsuko");
		eqenu(l,"(enum-set-difference a3 ph)", "ritsuko", "azusa", "iori");
		eqenu(l,"(enum-set-difference a3  e)");
		eqenu(l,"(enum-set-difference a4 a1)", "azusa");
		eqenu(l,"(enum-set-difference a4 a2)");
		eqenu(l,"(enum-set-difference a4 a3)");
		eqenu(l,"(enum-set-difference a4 a4)");
		eqenu(l,"(enum-set-difference a4 ph)", "azusa", "iori");
		eqenu(l,"(enum-set-difference a4  e)");
		eqenu(l,"(enum-set-difference ph a1)");
		eqenu(l,"(enum-set-difference ph a2)");
		eqenu(l,"(enum-set-difference ph a3)");
		eqenu(l,"(enum-set-difference ph a4)");
		eqenu(l,"(enum-set-difference ph ph)");
		eqenu(l,"(enum-set-difference ph  e)");
		eqenu(l,"(enum-set-difference  e a1)", "ritsuko", "azusa");
		eqenu(l,"(enum-set-difference  e a2)", "ritsuko");
		eqenu(l,"(enum-set-difference  e a3)", "ami");
		eqenu(l,"(enum-set-difference  e a4)", "ritsuko", "ami");
		eqenu(l,"(enum-set-difference  e ph)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-difference  e  e)");

		l.input("(define c1 (ce '(iori)))");
		l.input("(define c2 (cr '(iori)))");
		lperr(l,"(enum-set-difference c1 c2)");
		lperr(l,"(enum-set-difference c2 c1)");
	}

	public void testComplement() {
		Scheme l = Scheme.newInstance();

		l.input("(define e (make-enumeration '(ritsuko azusa iori ami)))");
		l.input("(define ce (enum-set-constructor e))");

		l.input("(define a1 (ce '(ami iori)))");
		l.input("(define a2 (ce '(azusa ami iori)))");
		l.input("(define a3 (ce '(ritsuko azusa iori)))");
		l.input("(define a4 (ce '(azusa iori)))");
		l.input("(define ph (ce '()))");
		eqenu(l,"(enum-set-complement a1)", "ritsuko", "azusa");
		eqenu(l,"(enum-set-complement a2)", "ritsuko");
		eqenu(l,"(enum-set-complement a3)", "ami");
		eqenu(l,"(enum-set-complement a4)", "ritsuko", "ami");
		eqenu(l,"(enum-set-complement ph)", "ritsuko", "azusa", "iori", "ami");
		eqenu(l,"(enum-set-complement  e)");
	}

}
