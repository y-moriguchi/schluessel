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
package net.morilib.util.io;

import junit.framework.TestCase;

public class FileAttributeFinderTest extends TestCase {

	static String c1(String s) {
		return FileAttributeFinder.compile(s).toString();
	}

	static void e1(String s) {
		try {
			FileAttributeFinder.compile(s);
			fail();
		} catch(FileAttributeFinderSyntaxException e) {}
	}

	public void testHidden() {
		assertEquals(c1("hidden"), "hidden");
	}

	public void testMtime() {
		assertEquals(c1("mtime==2012-12-10"), "mtime=2012/12/10");
		assertEquals(c1("m <=	2012-12-10"), "mtime<=2012/12/10");
		assertEquals(c1("modified-time > 2012-12-10"), "mtime>2012/12/10");
	}

	public void testPermission() {
		assertEquals(c1("perm<=7"), "perm<=7");
		assertEquals(c1("permission == 4"), "perm=4");
		assertEquals(c1("p == r"), "perm=4");
		assertEquals(c1("p == w"), "perm=2");
		assertEquals(c1("p == x"), "perm=1");
	}

	public void testAnd() {
		assertEquals(c1("perm<=7 & hidden"), "(perm<=7&hidden)");
		assertEquals(c1("perm<=7 & hidden & mtime=2012-12-10"),
				"(perm<=7&hidden&mtime=2012/12/10)");
	}

	public void testOr() {
		assertEquals(c1("perm<=7 | hidden"), "(perm<=7|hidden)");
		assertEquals(c1("perm<=7 | hidden | mtime=2012-12-10"),
				"(perm<=7|hidden|mtime=2012/12/10)");
		assertEquals(c1("perm<=7 & hidden | perm=4 & hidden"),
				"((perm<=7&hidden)|(perm=4&hidden))");
	}

	public void testNot() {
		assertEquals(c1("!perm<=7"), "!perm<=7");
		assertEquals(c1("hidden&!perm<=7"), "(hidden&!perm<=7)");
	}

	public void testParenthesis() {
		assertEquals(c1("(perm<=7 | hidden) & perm <= 4"),
				"((perm<=7|hidden)&perm<=4)");
		assertEquals(c1("(perm<=7 | hidden) & (perm <= 4)"),
				"((perm<=7|hidden)&perm<=4)");
		assertEquals(c1("!(hidden&perm<=7)"), "!(hidden&perm<=7)");
		assertEquals(c1("(hidden)"), "hidden");
		assertEquals(c1("((hidden))"), "hidden");
		assertEquals(c1("(hidden)"), "hidden");
		assertEquals(c1("((((hidden&hidden))))"), "(hidden&hidden)");
		assertEquals(c1("((hidden&hidden))"), "(hidden&hidden)");
		assertEquals(c1("((hidden|hidden))"), "(hidden|hidden)");
		assertEquals(c1("((!hidden))"), "!hidden");
		assertEquals(c1("!((hidden))"), "!hidden");
		assertEquals(c1("(hidden|!(hidden))"), "(hidden|!hidden)");
		assertEquals(c1("(hidden&!(hidden))"), "(hidden&!hidden)");
		assertEquals(c1("((hidden)|!hidden)"), "(hidden|!hidden)");
		assertEquals(c1("((hidden)&!hidden)"), "(hidden&!hidden)");
		assertEquals(c1("(!(hidden)|!hidden)"), "(!hidden|!hidden)");
		assertEquals(c1("(!(hidden)&!hidden)"), "(!hidden&!hidden)");
		assertEquals(c1("hidden|!(hidden)"), "(hidden|!hidden)");
		assertEquals(c1("hidden&!(hidden)"), "(hidden&!hidden)");
		assertEquals(c1("(hidden)|!hidden"), "(hidden|!hidden)");
		assertEquals(c1("(hidden)&!hidden"), "(hidden&!hidden)");
		assertEquals(c1("!(hidden)|!hidden"), "(!hidden|!hidden)");
		assertEquals(c1("!(hidden)&!hidden"), "(!hidden&!hidden)");
	}

	public void testError() {
		e1("hidden=1");
		e1("perm==17");
		e1("perm-=17");
		e1("mtime<=200-11-11");
		e1("mtime<=20121120");
		e1("mtime~=2012-11-20");
		e1("");
		try {
			FileAttributeFinder.compile(null);  fail();
		} catch(NullPointerException e) {}
	}

	public void testErrorOperator() {
		e1("hidden&");
		e1("hidden|");
		e1("hidden&perm<7&");
		e1("hidden|perm<7|");
		e1("hidden&)");
		e1("hidden|)");
		e1("hidden&hidden|hidden&hidden&)");
		e1("hidden&hidden|hidden&hidden|)");
		e1("!");
		e1("hidden&!");
		e1("hidden|!");
	}

	public void testUnbalancedParenthesis() {
		e1("(hidden&hidden");
		e1("(hidden&(hidden)");
		e1("((hidden)");
		e1("(hidden&hidden))");
		e1("(hidden&(hidden)))");
		e1("((hidden)))");
		e1("hidden)");
		e1("hidden|hidden)");
		e1("hidden&hidden)");
		e1("(hidden&hidden|hidden&hidden))");
		e1("((hidden&hidden)|(hidden&hidden)))");
		e1("hidden&hidden|(hidden&hidden))");
		e1("hidden&hidden)|hidden&hidden");
		e1("hidden&hidden)&hidden");
		e1("hidden&hidden|hidden)");
		e1("hidden&(hidden");
		e1("hidden|(hidden");
		e1("hidden&!(hidden");
		e1("hidden|!(hidden");
	}

}
