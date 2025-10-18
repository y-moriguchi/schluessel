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

import net.morilib.lisp.LispFloat;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.array.LispArrayC32;
import net.morilib.lisp.array.LispArrayC64;
import net.morilib.lisp.array.LispArrayR32;
import net.morilib.lisp.array.LispArrayR64;
import net.morilib.lisp.array.LispArrayS16;
import net.morilib.lisp.array.LispArrayS32;
import net.morilib.lisp.array.LispArrayS64;
import net.morilib.lisp.array.LispArrayS8;
import net.morilib.lisp.array.LispArrayT1;
import net.morilib.lisp.array.LispArrayU16;
import net.morilib.lisp.array.LispArrayU32;
import net.morilib.lisp.array.LispArrayU64;
import net.morilib.lisp.array.LispArrayU8;
import net.morilib.lisp.array.LispCharArray;
import net.morilib.lisp.array.LispDefaultArray;
import net.morilib.lisp.array.LispRank0Array;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/28
 */
public class SRFI58Test extends TCSubr {

	public void testMakeArray() {
		Scheme l = Scheme.newInstance();

		l.exec ("(define a #3A2*2*2:floC64b" +
				"  (((1+i 2+i) (3+i 4+i)) ((5+i 6+i) (7+i 8+i))))");
		ok(l.exec("a") instanceof LispArrayC64);
		equal(l,"(array-ref a 0 0 0)", newC(1.0, 1.0));
		equal(l,"(array-ref a 0 0 1)", newC(2.0, 1.0));
		equal(l,"(array-ref a 0 1 0)", newC(3.0, 1.0));
		equal(l,"(array-ref a 0 1 1)", newC(4.0, 1.0));
		equal(l,"(array-ref a 1 0 0)", newC(5.0, 1.0));
		equal(l,"(array-ref a 1 0 1)", newC(6.0, 1.0));
		equal(l,"(array-ref a 1 1 0)", newC(7.0, 1.0));
		equal(l,"(array-ref a 1 1 1)", newC(8.0, 1.0));

		l.exec ("(define a #3A2*2*2:floC32b" +
				"  (((1+i 2+i) (3+i 4+i)) ((5+i 6+i) (7+i 8+i))))");
		ok(l.exec("a") instanceof LispArrayC32);
		equal(l,"(array-ref a 0 0 0)", newC(1.0f, 1.0f));
		equal(l,"(array-ref a 0 0 1)", newC(2.0f, 1.0f));
		equal(l,"(array-ref a 0 1 0)", newC(3.0f, 1.0f));
		equal(l,"(array-ref a 0 1 1)", newC(4.0f, 1.0f));
		equal(l,"(array-ref a 1 0 0)", newC(5.0f, 1.0f));
		equal(l,"(array-ref a 1 0 1)", newC(6.0f, 1.0f));
		equal(l,"(array-ref a 1 1 0)", newC(7.0f, 1.0f));
		equal(l,"(array-ref a 1 1 1)", newC(8.0f, 1.0f));

		l.exec ("(define a #3A2*2*2:floR64b" +
				"  (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayR64);
		equal(l,"(array-ref a 0 0 0)", newR(1.0));
		equal(l,"(array-ref a 0 0 1)", newR(2.0));
		equal(l,"(array-ref a 0 1 0)", newR(3.0));
		equal(l,"(array-ref a 0 1 1)", newR(4.0));
		equal(l,"(array-ref a 1 0 0)", newR(5.0));
		equal(l,"(array-ref a 1 0 1)", newR(6.0));
		equal(l,"(array-ref a 1 1 0)", newR(7.0));
		equal(l,"(array-ref a 1 1 1)", newR(8.0));

		l.exec ("(define a #3A2*2*2:floR32b" +
				"  (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayR32);
		equal(l,"(array-ref a 0 0 0)", new LispFloat(1));
		equal(l,"(array-ref a 0 0 1)", new LispFloat(2));
		equal(l,"(array-ref a 0 1 0)", new LispFloat(3));
		equal(l,"(array-ref a 0 1 1)", new LispFloat(4));
		equal(l,"(array-ref a 1 0 0)", new LispFloat(5));
		equal(l,"(array-ref a 1 0 1)", new LispFloat(6));
		equal(l,"(array-ref a 1 1 0)", new LispFloat(7));
		equal(l,"(array-ref a 1 1 1)", new LispFloat(8));

		l.exec ("(define a #3A2*2*2:fixZ64b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayS64);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixZ32b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayS32);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixZ16b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayS16);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixZ8b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayS8);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixN64b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayU64);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixN32b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayU32);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixN16b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayU16);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:fixN8b (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispArrayU8);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A2*2*2:char" +
				" (((#\\1 #\\2) (#\\3 #\\4))" +
				"  ((#\\5 #\\6) (#\\7 #\\8))))");
		ok(l.exec("a") instanceof LispCharArray);
		equal(l,"(array-ref a 0 0 0)", chr('1'));
		equal(l,"(array-ref a 0 0 1)", chr('2'));
		equal(l,"(array-ref a 0 1 0)", chr('3'));
		equal(l,"(array-ref a 0 1 1)", chr('4'));
		equal(l,"(array-ref a 1 0 0)", chr('5'));
		equal(l,"(array-ref a 1 0 1)", chr('6'));
		equal(l,"(array-ref a 1 1 0)", chr('7'));
		equal(l,"(array-ref a 1 1 1)", chr('8'));

		l.exec ("(define a #3A2*2*2:bool" +
				" (((#t #f) (#t #f)) ((#t #f) (#t #f))))");
		ok(l.exec("a") instanceof LispArrayT1);
		equal(l,"(array-ref a 0 0 0)", T);
		equal(l,"(array-ref a 0 0 1)", F);
		equal(l,"(array-ref a 0 1 0)", T);
		equal(l,"(array-ref a 0 1 1)", F);
		equal(l,"(array-ref a 1 0 0)", T);
		equal(l,"(array-ref a 1 0 1)", F);
		equal(l,"(array-ref a 1 1 0)", T);
		equal(l,"(array-ref a 1 1 1)", F);

		l.exec ("(define a #3A2*2*2 (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #2*2*2 (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #A2*2*2 (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #3A (((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #A(((1 2) (3 4)) ((5 6) (7 8))))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0 0 0)", newZ(1));
		equal(l,"(array-ref a 0 0 1)", newZ(2));
		equal(l,"(array-ref a 0 1 0)", newZ(3));
		equal(l,"(array-ref a 0 1 1)", newZ(4));
		equal(l,"(array-ref a 1 0 0)", newZ(5));
		equal(l,"(array-ref a 1 0 1)", newZ(6));
		equal(l,"(array-ref a 1 1 0)", newZ(7));
		equal(l,"(array-ref a 1 1 1)", newZ(8));

		l.exec ("(define a #1A(1 2))");
		ok(l.exec("a") instanceof LispDefaultArray);
		equal(l,"(array-ref a 0)", newZ(1));
		equal(l,"(array-ref a 1)", newZ(2));

		l.exec ("(define a #0A 1)");
		ok(l.exec("a") instanceof LispRank0Array);
		equal(l,"(array-ref a)", newZ(1));

		lperr(l,"#2A2(1 2)");
		lperr(l,"#2A(1 2)");
		lperr(l,"#2A((1 2 3) (4 2))");
		lperr(l,"#2A(((1 1) (2 2)) ((4 2) (2 2)))");
		lperr(l,"#2A(((1 2) 3) (4 2))");
		lperr(l,"#3A(((1 2) 3) (4 2))");
	}

	public void testDisplay() {
		Scheme l = Scheme.newInstance();

		System.out.println(LispUtils.print(l.exec ("#3A2*2*2:floC64b" +
				"  (((1+i 2+i) (3+i 4+i)) ((5+i 6+i) (7+i 8+i)))")));
		System.out.println(LispUtils.print(l.exec ("#3A2*2*2:floC32b" +
				"  (((1+i 2+i) (3+i 4+i)) ((5+i 6+i) (7+i 8+i)))")));
		System.out.println(LispUtils.print(l.exec ("#3A2*2*2:floR64b" +
				"  (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(l.exec ("#3A2*2*2:floR32b" +
				"  (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixZ64b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixZ32b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixZ16b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixZ8b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixN64b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixN32b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixN16b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:fixN8b (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:char" +
						" (((#\\1 #\\2) (#\\3 #\\4))" +
						"  ((#\\5 #\\6) (#\\7 #\\8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2:bool" +
						" (((#t #f) (#t #f)) ((#t #f) (#t #f)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A2*2*2 (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#2*2*2 (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#A2*2*2 (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#3A (((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(
				l.exec ("#A(((1 2) (3 4)) ((5 6) (7 8)))")));
		System.out.println(LispUtils.print(l.exec ("#1A(1 2)")));
		System.out.println(LispUtils.print(l.exec ("#0A 1")));
		System.out.println(LispUtils.print(l.exec ("#0A:fixN8b 1")));
	}

}
