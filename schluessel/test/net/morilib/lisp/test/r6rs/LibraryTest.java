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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

public class LibraryTest extends TCSubr {
	
	public void testLibrary1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2) (ex4 ex04))" +
				"              ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		lperr(l,"(library (lib1) " + 
				"  (export ex1 1) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100))");
		lperr(l,"(library (lib1) " + 
				"  (export ex1 (ex2 ex3 ex4)) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100))");
		lperr(l,"(library (lib1) " + 
				"  (export ex1 (rename (ex2 ex3 ex4))) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100))");
		lperr(l,"(library (lib1) " + 
				"  (export ex1 (rename ex2)) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100))");
		lperr(l,"(library (lib1) " + 
				"  (export ex1 (rename (ex2))) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100))");
	}
	
	public void testImport1() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (lib1))");
		eqi  (l,"(ex1 5)", 25);
		eqi  (l,"(ex2 5)", 10);
		eqi  (l,"ex4", 100);
		eqi  (l,"ex5", 200);
		eqi  (l,"ex6", 300);
		eqi  (l,"ex7", 400);
		lperr(l,"ex3");
	}
	
	public void testImport2() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (library (lib1)))");
		eqi  (l,"(ex1 5)", 25);
		eqi  (l,"(ex2 5)", 10);
		eqi  (l,"ex4", 100);
		eqi  (l,"ex5", 200);
		eqi  (l,"ex6", 300);
		eqi  (l,"ex7", 400);
		lperr(l,"ex3");
	}
	
	public void testImport3() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (only (lib1) ex2 ex4))");
		eqi  (l,"(ex2 5)", 10);
		eqi  (l,"ex4", 100);
		lperr(l,"ex1");  lperr(l,"ex3");  lperr(l,"ex5");
		lperr(l,"ex6");  lperr(l,"ex7");
	}
	
	public void testImport4() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (except (lib1) ex2 ex5))");
		eqi  (l,"(ex1 5)", 25);
		eqi  (l,"ex4", 100);
		eqi  (l,"ex6", 300);
		eqi  (l,"ex7", 400);
		lperr(l,"ex2");  lperr(l,"ex3");  lperr(l,"ex5");
	}
	
	public void testImport5() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (rename (lib1) (ex2 ex02) (ex5 ex05)))");
		eqi  (l,"(ex1 5)", 25);
		eqi  (l,"(ex02 5)", 10);
		eqi  (l,"ex4", 100);
		eqi  (l,"ex05", 200);
		eqi  (l,"ex6", 300);
		eqi  (l,"ex7", 400);
		lperr(l,"ex2");  lperr(l,"ex3");  lperr(l,"ex5");
	}
	
	public void testImport6() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(import (rename" +
				"          (only (lib1) ex2 ex5)" +
				"          (ex2 ex02) (ex5 ex05)))");
		eqi  (l,"(ex02 5)", 10);
		eqi  (l,"ex05", 200);
		lperr(l,"ex1");  lperr(l,"ex2");  lperr(l,"ex3");
		lperr(l,"ex4");  lperr(l,"ex5");  lperr(l,"ex6");
		lperr(l,"ex7");
	}
	
	public void testImport7() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(library (lib2) " + 
				"  (export ez1 ez2 ez3 ez4) " +
				"  (define ez1 110) " +
				"  (define ez2 210) " +
				"  (define ez3 310) " +
				"  (define ez4 410))");
		l.exec ("(import (rename" +
				"          (only (lib1) ex2 ex5)" +
				"          (ex2 ex02) (ex5 ex05))" +
				"        (lib2))");
		eqi  (l,"(ex02 5)", 10);
		eqi  (l,"ex05", 200);
		lperr(l,"ex1");  lperr(l,"ex2");  lperr(l,"ex3");
		lperr(l,"ex4");  lperr(l,"ex5");  lperr(l,"ex6");
		lperr(l,"ex7");
		eqi  (l,"ez1", 110);
		eqi  (l,"ez2", 210);
		eqi  (l,"ez3", 310);
		eqi  (l,"ez4", 410);
	}
	
	public void testImport8() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1 (rename (ex3 ex2)) ex4 ex5 ex6 ex7) " +
				"  (define (ex1 x) (ex2 x)) " +
				"  (define (ex2 x) (* x x)) " +
				"  (define (ex3 x) (+ x x)) " +
				"  (define ex4 100) " +
				"  (define ex5 200) " +
				"  (define ex6 300) " +
				"  (define ex7 400))");
		l.exec ("(library (lib2) " + 
				"  (export ez1 ez2 ez3 ez4) " +
				"  (import (only (lib1) ex1 ex4))" +
				"  (define ez1 110) " +
				"  (define ez2 210) " +
				"  (define ez3 310) " +
				"  (define ez4 410))");
		l.exec ("(import (lib2))");
		eqi  (l,"(ex1 5)", 25);
		eqi  (l,"ex4", 100);
		lperr(l,"ex2");  lperr(l,"ex3");  lperr(l,"ex5");
		lperr(l,"ex6");  lperr(l,"ex7");
		eqi  (l,"ez1", 110);
		eqi  (l,"ez2", 210);
		eqi  (l,"ez3", 310);
		eqi  (l,"ez4", 410);
	}
	
	public void testImport9() {
		Scheme l = Scheme.newInstance();
		
		l.exec ("(library (lib1) " + 
				"  (export ex1) " +
				"  (define-syntax ex1" +
				"    (syntax-rules ()" +
				"      ((_ a) (list 'a)))))");
		l.exec ("(import (lib1))");
		equal(l,"(ex1 aaa)", list(sym("aaa")));
	}
	
}
