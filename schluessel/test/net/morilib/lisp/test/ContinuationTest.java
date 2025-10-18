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

import net.morilib.lisp.Scheme;

public class ContinuationTest extends TCSubr {
	
	public void testCallWithCC() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		eqi (l, "(call/cc (lambda (k)" +
				"          (+ 1 4 (k 3))))", 3);
		eqi (l, "(call/cc (lambda (k)" +
				"          (set! cont1 k)" +
				"          (+ 1 3 4)))", 8);
		eqi (l, "(cont1 4)", 4);
		eq  (l, "(call/cc pair?)", F);
		
		lperr(l, "(call/cc)");
		lperr(l, "(call/cc 1)");
	}
	
	public void testDynamicWind1() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define s1 '())");
		eqi (l, "(dynamic-wind " +
				"  (lambda ()  " +
				"    (set! s1 (append s1 '(1)))" +
				"    1)" +
				"  (lambda ()  " +
				"    (set! s1 (append s1 '(2)))" +
				"    (call/cc (lambda (k) (k 10) 2)))" +
				"  (lambda ()  " +
				"    (set! s1 (append s1 '(3)))" +
				"    3))", 10);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(dynamic-wind " +
				"  (lambda ()  " +
				"    (call/cc (lambda (k) (set! cont1 k))) " +
				"    (set! s1 (append s1 '(1)))" +
				"    1)" +
				"  (lambda ()  " +
				"    (call/cc (lambda (k) (set! cont2 k))) " +
				"    (set! s1 (append s1 '(2)))" +
				"    2)" +
				"  (lambda ()  " +
				"    (call/cc (lambda (k) (set! cont3 k))) " +
				"    (set! s1 (append s1 '(3)))" +
				"    3))", 2);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont1 #f)", 2);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont2 #f)", 2);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont3 #f)", 2);
		equal(l, "s1", list(3));
		l.input("(define s1 '())");
		//eqi (l, "(cont1 #f)", 2);
		//equal(l, "s1", list(1, 2, 3));
		//l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind " +
				"  (lambda ()  " +
				"    (let () (call/cc (lambda (k) (set! cont1 k)))) " +
				"    (set! s1 (append s1 '(1)))" +
				"    1)" +
				"  (lambda ()  " +
				"    (let () (call/cc (lambda (k) (set! cont2 k)))) " +
				"    (set! s1 (append s1 '(2)))" +
				//"    (call/cc (lambda (k) (k 10) 2)))" +
				"    10)" +
				"  (lambda ()  " +
				"    (let () (call/cc (lambda (k) (set! cont3 k)))) " +
				"    (set! s1 (append s1 '(3)))" +
				"    3))", 10);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont1 #f)", 10);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont2 #f)", 10);
		equal(l, "s1", list(1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont3 #f)", 10);
		equal(l, "s1", list(3));
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind + + +)", 0);
		
		//lperr(l, "(dynamic-wind)");
		//lperr(l, "(dynamic-wind + + 1)");
		//lperr(l, "(dynamic-wind + 1 +)");
		//lperr(l, "(dynamic-wind 1 + +)");
	}
	
	public void testDynamicWind2() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda ()" +
				"    (let () (call/cc (lambda (k) (set! cont5 k))))" +
				"    (set! s1 (append s1 '(5))))" +
				"   (lambda () (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(1, 4, 5, 6, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont5 #f)", 2);
		equal(l, "s1", list(1, 4, 5, 6, 2, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind3() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda ()" +
				"    (let () (call/cc (lambda (k) (set! cont4 k))))" +
				"    (set! s1 (append s1 '(5))))" +
				"   (lambda () (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(4, 5, 6, 1, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont4 #f)", 2);
		equal(l, "s1", list(4, 5, 6, 1, 2, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind4() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda ()" +
				"    (let () (call/cc (lambda (k) (set! cont6 k))))" +
				"    (set! s1 (append s1 '(5))))" +
				"   (lambda () (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(1, 2, 4, 5, 6, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont6 #f)", 2);
		equal(l, "s1", list(4, 5, 6, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind5() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda () (set! s1 (append s1 '(5))))" +
				"   (lambda ()" +
				"    (let () (call/cc (lambda (k) (set! cont6 k))))" +
				"    (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(1, 2, 4, 5, 6, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont6 #f)", 2);
		equal(l, "s1", list(6, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind10() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda ()" +
				"    (dynamic-wind" +
				"     (lambda () (set! s1 (append s1 '(7))))" +
				"     (lambda ()" +
				"      (let () (call/cc (lambda (k) (set! cont5 k))))" +
				"      (set! s1 (append s1 '(8))))" +
				"     (lambda () (set! s1 (append s1 '(9)))))" +
				"    (set! s1 (append s1 '(5))))" +
				"   (lambda () (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(1, 4, 7, 8, 9, 5, 6, 2, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont5 #f)", 2);
		equal(l, "s1", list(1, 4, 7, 8, 9, 5, 6, 2, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind11() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define cont1 #f)");
		l.input("(define cont2 #f)");
		l.input("(define cont3 #f)");
		l.input("(define cont4 #f)");
		l.input("(define cont5 #f)");
		l.input("(define cont6 #f)");
		l.input("(define s1 '())");
		
		eqi (l, "(dynamic-wind" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont1 k))))" +
				"  (set! s1 (append s1 '(1)))" +
				"  1)" +
				" (lambda ()" +
				"  (let () (call/cc (lambda (k) (set! cont2 k))))" +
				"  (set! s1 (append s1 '(2)))" +
				"  2)" +
				" (lambda ()" +
				"  (dynamic-wind" +
				"   (lambda () (set! s1 (append s1 '(4))))" +
				"   (lambda ()" +
				"    (dynamic-wind" +
				"     (lambda () (set! s1 (append s1 '(7))))" +
				"     (lambda () (set! s1 (append s1 '(8))))" +
				"     (lambda ()" +
				"      (let () (call/cc (lambda (k) (set! cont6 k))))" +
				"      (set! s1 (append s1 '(9)))))" +
				"    (set! s1 (append s1 '(5))))" +
				"   (lambda () (set! s1 (append s1 '(6)))))" +
				"  (let () (call/cc (lambda (k) (set! cont3 k))))" +
				"  (set! s1 (append s1 '(3)))" +
				"  3))", 2);
		equal(l, "s1", list(1, 2, 4, 7, 8, 9, 5, 6, 3));
		l.input("(define s1 '())");
		eqi (l, "(cont6 #f)", 2);
		equal(l, "s1", list(4, 9, 5, 6, 3));
		l.input("(define s1 '())");
	}
	
	public void testDynamicWind21() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define s1 '())");
		eqi (l, "(dynamic-wind " +
				"  (lambda () " +
				"    (set! s1 (append s1 '(1))) " +
				"    1) " +
				"  (lambda () " +
				"    (set! s1 (append s1 '(2))) " +
				"    (let ((cont #f)) " +
				"      (if (call/cc (lambda (k) (set! cont k) #t)) " +
				"          (dynamic-wind " +
				"           (lambda () (set! s1 (append s1 '(4)))) " +
				"           (lambda ()" +
				"             (set! s1 (append s1 '(5))) (cont #f)) " +
				"           (lambda () (set! s1 (append s1 '(6))))) " +
				"          10))) " +
				"  (lambda () " +
				"    (display 10)" +
				"    (set! s1 (append s1 '(3))) " +
				"    3))", 10);
		equal(l, "s1", list(1, 2, 4, 5, 6, 3));
		l.input("(define s1 '())");
	}
	
}
