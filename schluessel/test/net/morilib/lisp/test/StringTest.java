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
import net.morilib.lisp.Nil;

public class StringTest extends TCSubr {

	public void testIsString() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string? #f)", F);
		eq(l, "(string? 'a)", F);
		eq(l, "(string? #\\a)", F);
		eq(l, "(string? #(1 2))", F);
		eq(l, "(string? string?)", F);
		eq(l, "(string? '(1 2))", F);
		eq(l, "(string? 1)", F);
		eq(l, "(string? \"aaa\")", T);
		eq(l, "(string? (current-output-port))", F);
		lperr(l, "(string?)");
	}
	
	public void testStringLessThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string<? \"aaa\" \"abc\" \"acd\" \"add\")", T);
		eq(l, "(string<? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string<? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string<? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string<? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", F);
		eq(l, "(string<? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string<? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string<? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string<? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string<? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string<? \"aaa\" 'a)");
		lperr(l, "(string<? \"aaa\" '(3))");
		lperr(l, "(string<? \"aaa\")");
	}
	
	public void testStringLessThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string<=? \"aaa\" \"abc\" \"acd\" \"add\")", T);
		eq(l, "(string<=? \"aaa\" \"abc\" \"abc\" \"add\")", T);
		eq(l, "(string<=? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string<=? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string<=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string<=? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string<=? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string<=? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string<=? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string<=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string<=? \"aaa\" 'a)");
		lperr(l, "(string<=? \"aaa\" '(3))");
		lperr(l, "(string<=? \"aaa\")");
	}
	
	public void testStringMoreThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string>? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string>? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string>? \"add\" \"acd\" \"abc\" \"aaa\")", T);
		eq(l, "(string>? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string>? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", F);
		eq(l, "(string>? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string>? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string>? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string>? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string>? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string>? \"aaa\" 'a)");
		lperr(l, "(string>? \"aaa\" '(3))");
		lperr(l, "(string>? \"aaa\")");
	}
	
	public void testStringMoreThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string>=? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string>=? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string>=? \"add\" \"acd\" \"abc\" \"aaa\")", T);
		eq(l, "(string>=? \"add\" \"abc\" \"abc\" \"aaa\")", T);
		eq(l, "(string>=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string>=? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string>=? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string>=? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string>=? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string>=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string>=? \"aaa\" 'a)");
		lperr(l, "(string>=? \"aaa\" '(3))");
		lperr(l, "(string>=? \"aaa\")");
	}
	
	public void testStringEqual() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string=? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string=? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string=? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string=? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string=? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string=? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string=? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string=? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string=? \"aaa\" 'a)");
		lperr(l, "(string=? \"aaa\" '(3))");
		lperr(l, "(string=? \"aaa\")");
	}
	
	public void testStringCiLessThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string-ci<? \"aaa\" \"abc\" \"acd\" \"add\")", T);
		eq(l, "(string-ci<? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string-ci<? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci<? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci<? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", F);
		eq(l, "(string-ci<? \"aaa\" \"aBc\" \"Acd\" \"add\")", T);
		eq(l, "(string-ci<? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string-ci<? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string-ci<? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string-ci<? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string-ci<? \"aaa\" 'a)");
		lperr(l, "(string-ci<? \"aaa\" '(3))");
		lperr(l, "(string-ci<? \"aaa\")");
	}
	
	public void testStringCiLessThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string-ci<=? \"aaa\" \"abc\" \"acd\" \"add\")", T);
		eq(l, "(string-ci<=? \"aaa\" \"abc\" \"abc\" \"add\")", T);
		eq(l, "(string-ci<=? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci<=? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci<=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string-ci<=? \"aaa\" \"aBc\" \"Acd\" \"add\")", T);
		eq(l, "(string-ci<=? \"aaa\" \"aBc\" \"Abc\" \"add\")", T);
		eq(l, "(string-ci<=? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string-ci<=? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string-ci<=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", T);
		lperr(l, "(string-ci<=? \"aaa\" 'a)");
		lperr(l, "(string-ci<=? \"aaa\" '(3))");
		lperr(l, "(string-ci<=? \"aaa\")");
	}
	
	public void testStringCiMoreThan() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string-ci>? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string-ci>? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string-ci>? \"add\" \"acd\" \"abc\" \"aaa\")", T);
		eq(l, "(string-ci>? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci>? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", F);
		eq(l, "(string-ci>? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string-ci>? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string-ci>? \"add\" \"Acd\" \"aBc\" \"aaa\")", T);
		eq(l, "(string-ci>? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string-ci>? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", F);
		lperr(l, "(string-ci>? \"aaa\" 'a)");
		lperr(l, "(string-ci>? \"aaa\" '(3))");
		lperr(l, "(string-ci>? \"aaa\")");
	}
	
	public void testStringCiMoreThanEq() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string-ci>=? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string-ci>=? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string-ci>=? \"add\" \"acd\" \"abc\" \"aaa\")", T);
		eq(l, "(string-ci>=? \"add\" \"abc\" \"abc\" \"aaa\")", T);
		eq(l, "(string-ci>=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string-ci>=? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string-ci>=? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string-ci>=? \"add\" \"Acd\" \"aBc\" \"aaa\")", T);
		eq(l, "(string-ci>=? \"add\" \"aBc\" \"Abc\" \"aaa\")", T);
		eq(l, "(string-ci>=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", T);
		lperr(l, "(string-ci>=? \"aaa\" 'a)");
		lperr(l, "(string-ci>=? \"aaa\" '(3))");
		lperr(l, "(string-ci>=? \"aaa\")");
	}
	
	public void testStringCiEqual() {
		Scheme l = Scheme.newInstance();
		
		eq(l, "(string-ci=? \"aaa\" \"abc\" \"acd\" \"add\")", F);
		eq(l, "(string-ci=? \"aaa\" \"abc\" \"abc\" \"add\")", F);
		eq(l, "(string-ci=? \"add\" \"acd\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci=? \"add\" \"abc\" \"abc\" \"aaa\")", F);
		eq(l, "(string-ci=? \"aaa\" \"aaa\" \"aaa\" \"aaa\")", T);
		eq(l, "(string-ci=? \"aaa\" \"aBc\" \"Acd\" \"add\")", F);
		eq(l, "(string-ci=? \"aaa\" \"aBc\" \"Abc\" \"add\")", F);
		eq(l, "(string-ci=? \"add\" \"Acd\" \"aBc\" \"aaa\")", F);
		eq(l, "(string-ci=? \"add\" \"aBc\" \"Abc\" \"aaa\")", F);
		eq(l, "(string-ci=? \"aaa\" \"aAa\" \"Aaa\" \"aaA\")", T);
		lperr(l, "(string-ci=? \"aaa\" 'a)");
		lperr(l, "(string-ci=? \"aaa\" '(3))");
		lperr(l, "(string-ci=? \"aaa\")");
	}
	
	public void testString() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(string #\\a #\\b #\\c)", "abc");
		eqs(l, "(string)", "");
		lperr(l, "(string #\\a 'a)");
	}
	
	public void testMakeString() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(make-string 4 #\\a)", "aaaa");
		eqs(l, "(make-string 0 #\\a)", "");
		eqs(l, "(make-string 0)", "");
		lperr(l, "(make-string 'a #\\a)");
		lperr(l, "(make-string 4 'a)");
		lperr(l, "(make-string 'a)");
		lperr(l, "(make-string)");
	}
	
	public void testStringLength() {
		Scheme l = Scheme.newInstance();
		
		eqi(l, "(string-length \"aaaa\")", 4);
		eqi(l, "(string-length \"\")", 0);
		lperr(l, "(string-length 'a)");
		lperr(l, "(string-length)");
	}
	
	public void testStringRef() {
		Scheme l = Scheme.newInstance();
		
		eqc(l, "(string-ref \"abcd\" 1)", 'b');
		eqc(l, "(string-ref \"abcd\" 0)", 'a');
		eqc(l, "(string-ref \"abcd\" 3)", 'd');
		lperr(l, "(string-ref 'a 0)");
		lperr(l, "(string-ref \"abcd\" 4)");
		lperr(l, "(string-ref \"abcd\" -1)");
		lperr(l, "(string-ref \"abcd\" 1.0)");
		lperr(l, "(string-ref \"abcd\")");
	}
	
	public void testStringCopy() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(string-copy \"abcd\")", "abcd");
		lperr(l, "(string-copy 'a)");
		lperr(l, "(string-copy)");
	}
	
	public void testStringAppend() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(string-append \"abc\" \"def\" \"hgi\")", "abcdefhgi");
		eqs(l, "(string-append \"abc\")", "abc");
		eqs(l, "(string-append)", "");
		lperr(l, "(string-append \"abc\" 'def \"hgi\")");
	}
	
	public void testSubstring() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(substring \"abcdef\" 1 4)", "bcd");
		eqs(l, "(substring \"abcdef\" 0 6)", "abcdef");
		eqs(l, "(substring \"abcdef\" 4 4)", "");
		eqs(l, "(substring \"abcdef\" 4 3)", "");
		lperr(l, "(substring \"abcdef\" 4 7)");
		lperr(l, "(substring \"abcdef\" -1 4)");
		lperr(l, "(substring \"abcdef\" 4 5.0)");
		lperr(l, "(substring \"abcdef\" 1.0 4)");
		lperr(l, "(substring \"abcdef\" 4)");
	}
	
	public void testStringToList() {
		Scheme l = Scheme.newInstance();
		
		equal(l, "(string->list \"abc\")", list('a', 'b', 'c'));
		equal(l, "(string->list \"\")", Nil.NIL);
		lperr(l, "(string->list 'abc)");
		lperr(l, "(string->list)");
	}
	
	public void testListToString() {
		Scheme l = Scheme.newInstance();
		
		eqs(l, "(list->string '(#\\a #\\b #\\c))", "abc");
		eqs(l, "(list->string '())", "");
		lperr(l, "(list->string '(#\\a 'b #\\c))");
		lperr(l, "(list->string)");
	}
	
	public void testStringSetS() {
		Scheme l = Scheme.newInstance();
		
		l.exec("(define v \"abcd\")");
		l.exec("(string-set! v 1 #\\z)");
		equal(l.get("v"), str("azcd"));
		lperr(l, "(string-set! v -1 #\\c)");
		lperr(l, "(string-set! v 4 #\\z)");
		lperr(l, "(string-set! v 1.0 #\\z)");
		lperr(l, "(string-set! v 1 1)");
		lperr(l, "(string-set! '(1 2) 1 #\\z)");
		lperr(l, "(string-set! v 1)");
	}
	
}
