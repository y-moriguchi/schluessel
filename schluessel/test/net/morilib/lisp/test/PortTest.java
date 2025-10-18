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

import net.morilib.lisp.EOFObject;
import net.morilib.lisp.Scheme;

public class PortTest extends TCSubr {
	
	public void testIsInputPort() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-input-string \"test\"))");
		l.input("(define p2 (open-output-string))");
		eq(l, "(input-port? p1)", T);
		eq(l, "(input-port? p2)", F);
		eq(l, "(input-port? #t)", F);
		eq(l, "(input-port? 'a)", F);
		eq(l, "(input-port? #\\a)", F);
		eq(l, "(input-port? #(1 2))", F);
		eq(l, "(input-port? string?)", F);
		eq(l, "(input-port? '(1 2))", F);
		eq(l, "(input-port? 1)", F);
		eq(l, "(input-port? \"aaa\")", F);
		lperr(l, "(input-port?)");
	}
	
	public void testCloseInputPort() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-input-string \"test\"))");
		l.input("(close-input-port p1)");
		lperr(l,"(read p1)");
		lperr(l,"(read-char p1)");
		lperr(l,"(peek-char p1)");
		lperr(l,"(close-input-port (open-output-string))");
		lperr(l,"(close-input-port 1)");
	}
	
	public void testRead() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-input-string" +
				" \"(1 2 (3 4) 5) ( 6 7) 8 a\"))");
		equal(l,"(read p1)", list(1, 2, list(3, 4), 5));
		equal(l,"(read p1)", list(6, 7));
		eqi  (l,"(read p1)", 8);
		equal(l,"(read p1)", sym("a"));
		eq   (l,"(read p1)", EOFObject.EOF);
		
		l.input("(define p1 (open-input-string" +
				" \"8 ;abcd\"))");
		eqi  (l,"(read p1)", 8);
		eq   (l,"(read p1)", EOFObject.EOF);
		
		l.input("(define p1 (open-input-string" +
				" \"#| aaaa #| aaaa |# aaaaa|# 8\"))");
		eqi  (l,"(read p1)", 8);
		eq   (l,"(read p1)", EOFObject.EOF);
		
		l.input("(define p1 (open-input-string" +
				" \"8 #| aaaa #| bbb |# cccc |#\"))");
		eqi  (l,"(read p1)", 8);
		eq   (l,"(read p1)", EOFObject.EOF);
		
		l.input("(define p1 (open-input-string \"[1 2\"))");
		rderr(l,"(read p1)");
	}
	
	public void testReadChar() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-input-string \"ab(1 2 3)c\"))");
		equal(l,"(read-char p1)", chr('a'));
		equal(l,"(peek-char p1)", chr('b'));
		equal(l,"(read-char p1)", chr('b'));
		equal(l,"(read p1)", list(1, 2, 3));
		equal(l,"(read-char p1)", chr('c'));
		equal(l,"(read-char p1)", EOFObject.EOF);
		eq   (l,"(eof-object? (read-char p1))", T);
		eq   (l,"(eof-object? 'a)", F);
	}
	
	public void testIsOutputPort() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-input-string \"test\"))");
		l.input("(define p2 (open-output-string))");
		eq(l, "(output-port? p1)", F);
		eq(l, "(output-port? p2)", T);
		eq(l, "(output-port? #t)", F);
		eq(l, "(output-port? 'a)", F);
		eq(l, "(output-port? #\\a)", F);
		eq(l, "(output-port? #(1 2))", F);
		eq(l, "(output-port? string?)", F);
		eq(l, "(output-port? '(1 2))", F);
		eq(l, "(output-port? 1)", F);
		eq(l, "(output-port? \"aaa\")", F);
		lperr(l, "(output-port?)");
	}
	
	public void testCloseOutputPort() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-output-string))");
		l.input("(close-output-port p1)");
		lperr(l,"(write 1 p1)");
		lperr(l,"(display 1 p1)");
		lperr(l,"(newline p1)");
		lperr(l,"(close-output-port (open-input-string \"1\"))");
		lperr(l,"(close-output-port 1)");
	}
	
	public void testWrite() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-output-string))");
		l.input("(write '(1 2 3) p1)");
		l.input("(newline p1)");
		l.input("(write \"abc\" p1)");
		equal(l,"(get-output-string p1)", str("(1 2 3)" + System.lineSeparator() + "\"abc\""));
	}
	
	public void testDisplay() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-output-string))");
		l.input("(display '(1 2 3) p1)");
		l.input("(newline p1)");
		l.input("(display \"abc\" p1)");
		equal(l,"(get-output-string p1)", str("(1 2 3)" + System.lineSeparator() + "abc"));
	}
	
	public void testWriteChar() {
		Scheme l = Scheme.newInstance();
		
		l.input("(define p1 (open-output-string))");
		l.input("(write-char #\\a p1)");
		equal(l,"(get-output-string p1)", str("a"));
	}
	
}
