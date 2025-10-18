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

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.logging.Level;

import net.morilib.lisp.Nil;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.util.LispHandler;

public class JavaReflectTest extends TCSubr {
	
//	private static Logger _log = LogEnv.init("schlush.test.javarf");
	
	/* (non-Javadoc)
	 * @see junit.framework.TestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		LispHandler.setLoggable2(true);
		LispHandler.setLevel2(Level.FINER);
	}
	
	public void testSet1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(java-import stub1 net.morilib.lisp.test.StubClass1)");
		l.exec ("(define ins1 (java-new stub1))");
		l.exec ("(set! (stub1-bboolean    ins1) #t)");
		l.exec ("(set! (stub1-bchar       ins1) #\\a)");
		l.exec ("(set! (stub1-bint        ins1) 10)");
		l.exec ("(set! (stub1-blong       ins1) 102147483647)");
		l.exec ("(set! (stub1-bdouble     ins1) 1.5)");
		l.exec ("(set! (stub1-BBigInteger ins1) 109223372036854775807)");
		l.exec ("(set! (stub1-BBigDecimal ins1) 1/4)");
		l.exec ("(set! (stub1-BString     ins1) \"abc\")");
		l.exec ("(set! (stub1-biboolean    ins1 2) #t)");
		l.exec ("(set! (stub1-bichar       ins1 2) #\\a)");
		l.exec ("(set! (stub1-biint        ins1 2) 10)");
		l.exec ("(set! (stub1-bilong       ins1 2) 102147483647)");
		l.exec ("(set! (stub1-bidouble     ins1 2) 1.5)");
		l.exec ("(set! (stub1-biBigInteger ins1 2) 109223372036854775807)");
		l.exec ("(set! (stub1-biBigDecimal ins1 2) 1/4)");
		l.exec ("(set! (stub1-biString     ins1 2) \"abc\")");
		
		StubClass1 s = (StubClass1)l.getJavaInstance("ins1");
		eq(s.isBboolean(),     true);
		eq(s.getBchar(),       'a');
		eq(s.getBint(),        10);
		eq(s.getBlong(),       102147483647L);
		eq(s.getBdouble(),     1.5);
		eq(s.getBBigInteger(), new BigInteger("109223372036854775807"));
		eq(s.getBBigDecimal(), new BigDecimal("0.25"));
		eq(s.getBString(),     "abc");
		eq(s.getBiboolean(2),    true);
		eq(s.getBichar(2),       'a');
		eq(s.getBiint(2),        10);
		eq(s.getBilong(2),       102147483647L);
		eq(s.getBidouble(2),     1.5);
		eq(s.getBiBigInteger(2), new BigInteger("109223372036854775807"));
		eq(s.getBiBigDecimal(2), new BigDecimal("0.25"));
		eq(s.getBiString(2),     "abc");
		
		l.exec ("(set! (stub1-bilong       ins1 3) 10)");
		l.exec ("(set! (stub1-bidouble     ins1 3) 10)");
		l.exec ("(set! (stub1-biBigInteger ins1 3) 10)");
		l.exec ("(set! (stub1-biBigDecimal ins1 3) 10)");
		l.exec ("(set! (stub1-bidouble     ins1 4) 3/2)");
		l.exec ("(set! (stub1-biBigInteger ins1 4) 102147483647)");
		l.exec ("(set! (stub1-biBigDecimal ins1 4) 0.25)");
		eq(s.getBilong(3),       10L);
		eq(s.getBidouble(3),     10.0);
		eq(s.getBiBigInteger(3), new BigInteger("10"));
		eq(s.getBiBigDecimal(3), new BigDecimal("10"));
		eq(s.getBidouble(4),     1.5);
		eq(s.getBiBigInteger(4), new BigInteger("102147483647"));
		eq(s.getBiBigDecimal(4), new BigDecimal("0.25"));
		
		l.exec ("(set! (stub1-strlist ins1)" +
				" '(\"aaa\" \"bbb\" \"ccc\" \"ddd\"))");
		String[] sa = s.getStrlist();
		eq(sa[0], "aaa");
		eq(sa[1], "bbb");
		eq(sa[2], "ccc");
		eq(sa[3], "ddd");
		
		l.exec ("(set! (stub1-matrix ins1)" +
				" '((1 2 3) (4 5 6) (7 8 9)))");
		int[][] mt = s.getMatrix();
		eq(mt.length, 3);
		eq(mt[0].length, 3);
		eq(mt[1].length, 3);
		eq(mt[2].length, 3);
		eq(mt[0][0], 1);  eq(mt[0][1], 2);  eq(mt[0][2], 3);
		eq(mt[1][0], 4);  eq(mt[1][1], 5);  eq(mt[1][2], 6);
		eq(mt[2][0], 7);  eq(mt[2][1], 8);  eq(mt[2][2], 9);
		
		l.exec ("(set! (stub1-strlist ins1) '())");
		eq(s.getStrlist().length, 0);
		l.exec ("(set! (stub1-matrix ins1)" +
				" '(() (1) (2 3)))");
		mt = s.getMatrix();
		eq(mt.length, 3);
		eq(mt[0].length, 0);
		eq(mt[1].length, 1);
		eq(mt[2].length, 2);
		eq(mt[1][0], 1);
		eq(mt[2][0], 2);  eq(mt[2][1], 3);
		
		//lperr(l,"(set! (stub1-bboolean    ins1) #t)");
		lperr(l,"(set! (stub1-bchar       ins1) #t)");
		lperr(l,"(set! (stub1-bint        ins1) #t)");
		lperr(l,"(set! (stub1-blong       ins1) #t)");
		lperr(l,"(set! (stub1-bdouble     ins1) #t)");
		lperr(l,"(set! (stub1-BBigInteger ins1) #t)");
		lperr(l,"(set! (stub1-BBigDecimal ins1) #t)");
		lperr(l,"(set! (stub1-BString     ins1) #t)");
		
		lperr(l,"(set! (stub1-bboolean    ins1) #\\a)");
		//lperr(l,"(set! (stub1-bchar       ins1) #\\a)");
		lperr(l,"(set! (stub1-bint        ins1) #\\a)");
		lperr(l,"(set! (stub1-blong       ins1) #\\a)");
		lperr(l,"(set! (stub1-bdouble     ins1) #\\a)");
		lperr(l,"(set! (stub1-BBigInteger ins1) #\\a)");
		lperr(l,"(set! (stub1-BBigDecimal ins1) #\\a)");
		lperr(l,"(set! (stub1-BString     ins1) #\\a)");
		
		lperr(l,"(set! (stub1-bboolean    ins1) 10)");
		lperr(l,"(set! (stub1-bchar       ins1) 10)");
		//lperr(l,"(set! (stub1-bint        ins1) 10)");
		//lperr(l,"(set! (stub1-blong       ins1) 10)");
		//lperr(l,"(set! (stub1-bdouble     ins1) 10)");
		//lperr(l,"(set! (stub1-BBigInteger ins1) 10)");
		//lperr(l,"(set! (stub1-BBigDecimal ins1) 10)");
		lperr(l,"(set! (stub1-BString     ins1) 10)");
		
		lperr(l,"(set! (stub1-bboolean    ins1) \"abc\")");
		lperr(l,"(set! (stub1-bchar       ins1) \"abc\")");
		lperr(l,"(set! (stub1-bint        ins1) \"abc\")");
		lperr(l,"(set! (stub1-blong       ins1) \"abc\")");
		lperr(l,"(set! (stub1-bdouble     ins1) \"abc\")");
		lperr(l,"(set! (stub1-BBigInteger ins1) \"abc\")");
		lperr(l,"(set! (stub1-BBigDecimal ins1) \"abc\")");
		//lperr(l,"(set! (stub1-BString     ins1) \"abc\")");
		
		//lperr(l,"(set! (stub1-biboolean    ins1 2) #t)");
		lperr(l,"(set! (stub1-bichar       ins1 2) #t)");
		lperr(l,"(set! (stub1-biint        ins1 2) #t)");
		lperr(l,"(set! (stub1-bilong       ins1 2) #t)");
		lperr(l,"(set! (stub1-bidouble     ins1 2) #t)");
		lperr(l,"(set! (stub1-biBigInteger ins1 2) #t)");
		lperr(l,"(set! (stub1-biBigDecimal ins1 2) #t)");
		lperr(l,"(set! (stub1-biString     ins1 2) #t)");
		
		lperr(l,"(set! (stub1-biboolean    ins1 2) #\\a)");
		//lperr(l,"(set! (stub1-bichar       ins1 2) #\\a)");
		lperr(l,"(set! (stub1-biint        ins1 2) #\\a)");
		lperr(l,"(set! (stub1-bilong       ins1 2) #\\a)");
		lperr(l,"(set! (stub1-bidouble     ins1 2) #\\a)");
		lperr(l,"(set! (stub1-biBigInteger ins1 2) #\\a)");
		lperr(l,"(set! (stub1-biBigDecimal ins1 2) #\\a)");
		lperr(l,"(set! (stub1-biString     ins1 2) #\\a)");
		
		lperr(l,"(set! (stub1-biboolean    ins1 2) 10)");
		lperr(l,"(set! (stub1-bichar       ins1 2) 10)");
		//lperr(l,"(set! (stub1-biint        ins1 2) 10)");
		//lperr(l,"(set! (stub1-bilong       ins1 2) 10)");
		//lperr(l,"(set! (stub1-bidouble     ins1 2) 10)");
		//lperr(l,"(set! (stub1-biBigInteger ins1 2) 10)");
		//lperr(l,"(set! (stub1-biBigDecimal ins1 2) 10)");
		lperr(l,"(set! (stub1-biString     ins1 2) 10)");
		
		lperr(l,"(set! (stub1-biboolean    ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-bichar       ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-biint        ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-bilong       ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-bidouble     ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-biBigInteger ins1 2) \"abc\")");
		lperr(l,"(set! (stub1-biBigDecimal ins1 2) \"abc\")");
		//lperr(l,"(set! (stub1i-bString     ins1 2) \"abc\")");
	}
	
	public void testGet1() {
		Scheme l = Scheme.newInstance();
		StubClass1 s = new StubClass1();
		
		s.setBboolean    (true);
		s.setBchar       ('a');
		s.setBint        (10);
		s.setBlong       (102147483647L);
		s.setBdouble     (1.5);
		s.setBBigInteger (new BigInteger("109223372036854775807"));
		s.setBBigDecimal (new BigDecimal("0.25"));
		s.setBString     ("abc");
		s.setBiboolean   (2, true);
		s.setBichar      (2, 'a');
		s.setBiint       (2, 10);
		s.setBilong      (2, 102147483647L);
		s.setBidouble    (2, 1.5);
		s.setBiBigInteger(2, new BigInteger("109223372036854775807"));
		s.setBiBigDecimal(2, new BigDecimal("0.25"));
		s.setBiString    (2, "abc");
		
		l.exec ("(java-import stub1 net.morilib.lisp.test.StubClass1)");
		l.setJavaInstance("ins1", s);
		equal(l,"(stub1-bboolean    ins1)", T);
		equal(l,"(stub1-bchar       ins1)", chr('a'));
		equal(l,"(stub1-bint        ins1)", newZ(10));
		equal(l,"(stub1-blong       ins1)", newZ("102147483647"));
		equal(l,"(stub1-bdouble     ins1)", newR(1.5));
		equal(l,"(stub1-BBigInteger ins1)", newZ("109223372036854775807"));
		equal(l,"(stub1-BBigDecimal ins1)", newQ(1, 4));
		equal(l,"(stub1-BString     ins1)", str("abc"));
		equal(l,"(stub1-biboolean    ins1 2)", T);
		equal(l,"(stub1-bichar       ins1 2)", chr('a'));
		equal(l,"(stub1-biint        ins1 2)", newZ(10));
		equal(l,"(stub1-bilong       ins1 2)", newZ("102147483647"));
		equal(l,"(stub1-bidouble     ins1 2)", newR(1.5));
		equal(l,"(stub1-biBigInteger ins1 2)",
				newZ("109223372036854775807"));
		equal(l,"(stub1-biBigDecimal ins1 2)", newQ(1, 4));
		equal(l,"(stub1-biString     ins1 2)", str("abc"));
		
		String[] sa = new String[] { "aaa", "bbb", "ccc", "ddd" };
		int[][]  mt = new int[][] {
				{ 1, 2, 3 },
				{ 4, 5, 6 },
				{ 7, 8, 9 }};
		s.setStrlist(sa);
		s.setMatrix(mt);
		equal(l,"(stub1-strlist ins1)",
				list(str("aaa"), str("bbb"), str("ccc"), str("ddd")));
		equal(l,"(stub1-matrix ins1)",
				list(list(1, 2, 3), list(4, 5, 6), list(7, 8, 9)));
		
		sa = new String[] {};
		mt = new int[][] {
				{ },
				{ 4 },
				{ 7, 8 }};
		s.setStrlist(sa);
		s.setMatrix(mt);
		equal(l,"(stub1-strlist ins1)", Nil.NIL);
		equal(l,"(stub1-matrix ins1)",
				list(Nil.NIL, list(4), list(7, 8)));
	}
	
//	public void testException1() {
//		Scheme l = Scheme.newInstance();
//		
//		_log.info("testException1 start");
//		l.exec ("(java-import stub1 net.morilib.lisp.test.StubClass1)");
//		l.exec ("(java-import excp0 java.lang.Exception)");
//		l.exec ("(define ins1 (java-new stub1))");
//		l.exec ("(set! (stub1-strlist ins1)" +
//				" '(\"a\" \"b\" \"c\"))");
//		equal(l,"(java-catch" +
//				"    (e (java.lang.IndexOutOfBoundsException 'error)" +
//				"       (java.lang.NullPointerException      'nullpo))" +
//				"  (stub1-str ins1 10)" +
//				"  'ok)", sym("error"));
//		equal(l,"(java-catch" +
//				"    (e (java.lang.NullPointerException      'nullpo)" +
//				"       (java.lang.IndexOutOfBoundsException 'error))" +
//				"  (stub1-str ins1 10)" +
//				"  'ok)", sym("error"));
//		equal(l,"(java-catch" +
//				"    (e (java.lang.NullPointerException 'nullpo)" +
//				"       (java.lang.IndexOutOfBoundsException" +
//				"         (java-catch" +
//				"             (e (java.lang.Exception 'error2))" +
//				"           (stub1-str ins1 10)" +
//				"           'ng1)))" +
//				"  (stub1-str ins1 10)" +
//				"  'ok)", sym("error2"));
//		
//		equal(l,"(call/cc" +
//				"  (lambda (k)" +
//				"    (let loop ((l '()) (c 0))" +
//				"      (java-catch" +
//				"          (e (java.lang.IndexOutOfBoundsException" +
//				"               (k (cons (excp0-message e) l))))" +
//				"        (loop (cons (stub1-str ins1 c) l) (+ c 1))))))",
//				list(str("3"), str("c"), str("b"), str("a")));
//		
//		l.exec ("(define cont1 #f)");
//		equal(l,"(java-catch" +
//				"    (e (java.lang.IndexOutOfBoundsException" +
//				"        'error))" +
//				"  (stub1-str ins1" +
//				"             (call/cc" +
//				"               (lambda (k)" +
//				"                 (set! cont1 k) 1))))", str("b"));
//		equal(l,"(cont1 10)", sym("error"));
//		_log.info("testException1 end");
//	}
	
}
