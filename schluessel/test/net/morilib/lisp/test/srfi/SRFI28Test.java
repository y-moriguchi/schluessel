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
package net.morilib.lisp.test.srfi;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

public class SRFI28Test extends TCSubr {
	
	public void testTotal1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"-~a-~s-~~~%\" 10 11)", str("-10-11-~\n"));
		equal(l,"(format \"~v,,Va\" 6 4 'aaa)", str("aaa    "));
		lperr(l,"(format \"~v,,Va\" 'aaa 6 4)");
	}
	
	public void testA1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~A\" 10)", str("10"));
		equal(l,"(format \"~A\" 'aaa)", str("aaa"));
		equal(l,"(format \"~A\" \"a\\\"aa\")", str("a\"aa"));
		equal(l,"(format \"~A\" 1/2)", str("1/2"));
		equal(l,"(format \"~a\" #t)", str("#t"));
		equal(l,"(format \"~a\" 1.2)", str("1.2"));
		equal(l,"(format \"~a\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~a\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~a\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5a\" 'aaa)", str("aaa  "));
		equal(l,"(format \"~5@a\" 'aaa)", str("  aaa"));
		equal(l,"(format \"~2a\" 'aaa)", str("aaa"));
		equal(l,"(format \"~6,3a\" 'aaa)", str("aaa   "));
		equal(l,"(format \"~5,3a\" 'aaa)", str("aaa   "));
		equal(l,"(format \"~4,3a\" 'aaa)", str("aaa   "));
		equal(l,"(format \"~7,3a\" 'aaa)", str("aaa      "));
		equal(l,"(format \"~6,,4a\" 'aaa)", str("aaa    "));
		equal(l,"(format \"~2,,4a\" 'aaa)", str("aaa    "));
		equal(l,"(format \"~2,3,4a\" 'aaa)", str("aaa    "));
		equal(l,"(format \"~5,3,4a\" 'aaa)", str("aaa    "));
		equal(l,"(format \"~,,4a\" 'aaa)", str("aaa    "));
		equal(l,"(format \"~,,4,'@a\" 'aaa)", str("aaa@@@@"));
		equal(l,"(format \"~,,4,'@@a\" 'aaa)", str("@@@@aaa"));
	}
	
	public void testS1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~S\" 10)", str("10"));
		equal(l,"(format \"~S\" 'aaa)", str("aaa"));
		equal(l,"(format \"~S\" \"a\\\"aa\")", str("\"a\\\"aa\""));
		equal(l,"(format \"~S\" 1/2)", str("1/2"));
		equal(l,"(format \"~s\" #t)", str("#t"));
		equal(l,"(format \"~s\" 1.2)", str("1.2"));
		equal(l,"(format \"~s\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~s\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~s\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5s\" \"a\")", str("\"a\"  "));
		equal(l,"(format \"~5@s\" \"a\")", str("  \"a\""));
		equal(l,"(format \"~2s\" \"a\")", str("\"a\""));
		equal(l,"(format \"~6,3s\" \"a\")", str("\"a\"   "));
		equal(l,"(format \"~5,3s\" \"a\")", str("\"a\"   "));
		equal(l,"(format \"~4,3s\" \"a\")", str("\"a\"   "));
		equal(l,"(format \"~7,3s\" \"a\")", str("\"a\"      "));
		equal(l,"(format \"~6,,4s\" \"a\")", str("\"a\"    "));
		equal(l,"(format \"~2,,4s\" \"a\")", str("\"a\"    "));
		equal(l,"(format \"~2,3,4s\" \"a\")", str("\"a\"    "));
		equal(l,"(format \"~5,3,4s\" \"a\")", str("\"a\"    "));
		equal(l,"(format \"~,,4s\" \"a\")", str("\"a\"    "));
		equal(l,"(format \"~,,4,'@s\" \"a\")", str("\"a\"@@@@"));
		equal(l,"(format \"~,,4,'@@s\" \"a\")", str("@@@@\"a\""));
	}
	
	public void testD1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~D\" 32)", str("32"));
		equal(l,"(format \"~D\" 'aaa)", str("aaa"));
		equal(l,"(format \"~D\" \"a\\\"aa\")", str("a\"aa"));
		equal(l,"(format \"~D\" 1/2)", str("1/2"));
		equal(l,"(format \"~d\" #t)", str("#t"));
		equal(l,"(format \"~d\" 1.2)", str("1.2"));
		equal(l,"(format \"~d\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~d\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~d\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5d\" 10)", str("   10"));
		equal(l,"(format \"~5d\" -10)", str("  -10"));
		equal(l,"(format \"~2d\" 1000)", str("1000"));
		equal(l,"(format \"~5,'0d\" 10)", str("00010"));
		equal(l,"(format \"~5@d\" 10)", str("  +10"));
		equal(l,"(format \"~5@d\" -10)", str("  -10"));
		equal(l,"(format \"~:d\" 12345)", str("12,345"));
		equal(l,"(format \"~7:d\" 12345)", str(" 12,345"));
		equal(l,"(format \"~7,'0:d\" 12345)", str("012,345"));
		equal(l,"(format \"~7,'0,' :d\" 12345)", str("012 345"));
		equal(l,"(format \"~7,'0,,4:d\" 12345)", str("01,2345"));
		equal(l,"(format \"~7,'0,' ,4:d\" 12345)", str("01 2345"));
		equal(l,"(format \"~:d\" -12345)", str("-12,345"));
		equal(l,"(format \"~8:d\" -12345)", str(" -12,345"));
		//equal(l,"(format \"~8,'0:d\" -12345)", str("-012,345"));
		//equal(l,"(format \"~8,'0,' :d\" -12345)", str("-012 345"));
		equal(l,"(format \"~9,'0:d\" -12345)", str("-012,345"));
		equal(l,"(format \"~9,'0,' :d\" -12345)", str("-012 345"));
		equal(l,"(format \"~8,'0,,4:d\" -12345)", str("-001,2345"));
		equal(l,"(format \"~8,'0,' ,4:d\" -12345)", str("-001 2345"));
		equal(l,"(format \"~8@:d\" 12345)", str(" +12,345"));
		equal(l,"(format \"~8@:d\" 12345)", str(" +12,345"));
		equal(l,"(format \"~8:@d\" -12345)", str(" -12,345"));
		equal(l,"(format \"~8:@d\" -12345)", str(" -12,345"));
	}
	
	public void testB1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~B\" 32)", str("100000"));
		equal(l,"(format \"~B\" 'aaa)", str("aaa"));
		equal(l,"(format \"~B\" \"a\\\"aa\")", str("a\"aa"));
		equal(l,"(format \"~B\" 1/2)", str("1/2"));
		equal(l,"(format \"~b\" #t)", str("#t"));
		equal(l,"(format \"~b\" 1.2)", str("1.2"));
		equal(l,"(format \"~b\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~b\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~b\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5b\" #b10)", str("   10"));
		equal(l,"(format \"~5b\" #b-10)", str("  -10"));
		equal(l,"(format \"~2b\" #b1000)", str("1000"));
		equal(l,"(format \"~5,'0b\" #b10)", str("00010"));
		equal(l,"(format \"~5@b\" #b10)", str("  +10"));
		equal(l,"(format \"~5@b\" #b-10)", str("  -10"));
		equal(l,"(format \"~:b\" #b10101)", str("10,101"));
		equal(l,"(format \"~7:b\" #b10101)", str(" 10,101"));
		equal(l,"(format \"~7,'0:b\" #b10101)", str("010,101"));
		equal(l,"(format \"~7,'0,' :b\" #b10101)", str("010 101"));
		equal(l,"(format \"~7,'0,,4:b\" #b10101)", str("01,0101"));
		equal(l,"(format \"~7,'0,' ,4:b\" #b10101)", str("01 0101"));
		equal(l,"(format \"~:b\" #b-10101)", str("-10,101"));
		equal(l,"(format \"~8:b\" #b-10101)", str(" -10,101"));
		//equal(l,"(format \"~8,'0:b\" -12345)", str("-012,345"));
		//equal(l,"(format \"~8,'0,' :b\" -12345)", str("-012 345"));
		equal(l,"(format \"~9,'0:b\" #b-10101)", str("-010,101"));
		equal(l,"(format \"~9,'0,' :b\" #b-10101)", str("-010 101"));
		equal(l,"(format \"~8,'0,,4:b\" #b-10101)", str("-001,0101"));
		equal(l,"(format \"~8,'0,' ,4:b\" #b-10101)", str("-001 0101"));
		equal(l,"(format \"~8@:b\" #b10101)", str(" +10,101"));
		equal(l,"(format \"~8@:b\" #b10101)", str(" +10,101"));
		equal(l,"(format \"~8:@b\" #b-10101)", str(" -10,101"));
		equal(l,"(format \"~8:@b\" #b-10101)", str(" -10,101"));
	}
	
	public void testO1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~O\" 32)", str("40"));
		equal(l,"(format \"~O\" 'aaa)", str("aaa"));
		equal(l,"(format \"~O\" \"a\\\"aa\")", str("a\"aa"));
		equal(l,"(format \"~O\" 1/2)", str("1/2"));
		equal(l,"(format \"~o\" #t)", str("#t"));
		equal(l,"(format \"~o\" 1.2)", str("1.2"));
		equal(l,"(format \"~o\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~o\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~o\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5o\" #o10)", str("   10"));
		equal(l,"(format \"~5o\" #o-10)", str("  -10"));
		equal(l,"(format \"~2o\" #o1000)", str("1000"));
		equal(l,"(format \"~5,'0o\" #o10)", str("00010"));
		equal(l,"(format \"~5@o\" #o10)", str("  +10"));
		equal(l,"(format \"~5@o\" #o-10)", str("  -10"));
		equal(l,"(format \"~:o\" #o12345)", str("12,345"));
		equal(l,"(format \"~7:o\" #o12345)", str(" 12,345"));
		equal(l,"(format \"~7,'0:o\" #o12345)", str("012,345"));
		equal(l,"(format \"~7,'0,' :o\" #o12345)", str("012 345"));
		equal(l,"(format \"~7,'0,,4:o\" #o12345)", str("01,2345"));
		equal(l,"(format \"~7,'0,' ,4:o\" #o12345)", str("01 2345"));
		equal(l,"(format \"~:o\" #o-12345)", str("-12,345"));
		equal(l,"(format \"~8:o\" #o-12345)", str(" -12,345"));
		//equal(l,"(format \"~8,'0:o\" -12345)", str("-012,345"));
		//equal(l,"(format \"~8,'0,' :o\" -12345)", str("-012 345"));
		equal(l,"(format \"~9,'0:o\" #o-12345)", str("-012,345"));
		equal(l,"(format \"~9,'0,' :o\" #o-12345)", str("-012 345"));
		equal(l,"(format \"~8,'0,,4:o\" #o-12345)", str("-001,2345"));
		equal(l,"(format \"~8,'0,' ,4:o\" #o-12345)", str("-001 2345"));
		equal(l,"(format \"~8@:o\" #o12345)", str(" +12,345"));
		equal(l,"(format \"~8@:o\" #o12345)", str(" +12,345"));
		equal(l,"(format \"~8:@o\" #o-12345)", str(" -12,345"));
		equal(l,"(format \"~8:@o\" #o-12345)", str(" -12,345"));
	}
	
	public void testX1() {
		Scheme l = Scheme.newInstance();
		
		equal(l,"(format \"~X\" 32)", str("20"));
		equal(l,"(format \"~X\" 'aaa)", str("aaa"));
		equal(l,"(format \"~X\" \"a\\\"aa\")", str("a\"aa"));
		equal(l,"(format \"~X\" 1/2)", str("1/2"));
		equal(l,"(format \"~x\" #t)", str("#t"));
		equal(l,"(format \"~x\" 1.2)", str("1.2"));
		equal(l,"(format \"~x\" '(1 2 3))", str("(1 2 3)"));
		equal(l,"(format \"~x\" '(1 (2 3) (4) . 5))",
				str("(1 (2 3) (4) . 5)"));
		equal(l,"(format \"~x\" '#(1 2 3))", str("#(1 2 3)"));
		equal(l,"(format \"~5x\" #x10)", str("   10"));
		equal(l,"(format \"~5x\" #x-10)", str("  -10"));
		equal(l,"(format \"~2x\" #x1000)", str("1000"));
		equal(l,"(format \"~5,'0x\" #x10)", str("00010"));
		equal(l,"(format \"~5@x\" #x10)", str("  +10"));
		equal(l,"(format \"~5@x\" #x-10)", str("  -10"));
		equal(l,"(format \"~:x\" #x12345)", str("12,345"));
		equal(l,"(format \"~7:x\" #x12345)", str(" 12,345"));
		equal(l,"(format \"~7,'0:x\" #x12345)", str("012,345"));
		equal(l,"(format \"~7,'0,' :x\" #x12345)", str("012 345"));
		equal(l,"(format \"~7,'0,,4:x\" #x12345)", str("01,2345"));
		equal(l,"(format \"~7,'0,' ,4:x\" #x12345)", str("01 2345"));
		equal(l,"(format \"~:x\" #x-12345)", str("-12,345"));
		equal(l,"(format \"~8:x\" #x-12345)", str(" -12,345"));
		//equal(l,"(format \"~8,'0:x\" #x-12345)", str("-012,345"));
		//equal(l,"(format \"~8,'0,' :x\" #x-12345)", str("-012 345"));
		equal(l,"(format \"~9,'0:x\" #x-12345)", str("-012,345"));
		equal(l,"(format \"~9,'0,' :x\" #x-12345)", str("-012 345"));
		equal(l,"(format \"~8,'0,,4:x\" #x-12345)", str("-001,2345"));
		equal(l,"(format \"~8,'0,' ,4:x\" #x-12345)", str("-001 2345"));
		equal(l,"(format \"~8@:x\" #x12345)", str(" +12,345"));
		equal(l,"(format \"~8@:x\" #x12345)", str(" +12,345"));
		equal(l,"(format \"~8:@x\" #x-12345)", str(" -12,345"));
		equal(l,"(format \"~8:@x\" #x-12345)", str(" -12,345"));
	}
	
}
