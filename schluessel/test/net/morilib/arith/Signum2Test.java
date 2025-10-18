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
package net.morilib.arith;

import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/09/23
 */
public class Signum2Test extends TC {

	public void testToSignum() {
		eq(Signum2.toSignum( 1), Signum2.POSITIVE);
		eq(Signum2.toSignum(-1), Signum2.NEGATIVE);
		try {
			Signum2.toSignum(0);  fail();
		} catch(IllegalArgumentException e) {}

		eq(Signum2.toSignum( 1l), Signum2.POSITIVE);
		eq(Signum2.toSignum(-1l), Signum2.NEGATIVE);
		try {
			Signum2.toSignum(0l);  fail();
		} catch(IllegalArgumentException e) {}
	}

	public void testMultiplyS() {
		eq(Signum2.POSITIVE.multiply(Signum2.POSITIVE), Signum2.POSITIVE);
		eq(Signum2.POSITIVE.multiply(Signum2.NEGATIVE), Signum2.NEGATIVE);
		eq(Signum2.NEGATIVE.multiply(Signum2.POSITIVE), Signum2.NEGATIVE);
		eq(Signum2.NEGATIVE.multiply(Signum2.NEGATIVE), Signum2.POSITIVE);
	}

	public void testSignum() {
		eq(Signum2.POSITIVE.signum(), 1);
		eq(Signum2.NEGATIVE.signum(), -1);
	}

	public void testMultiplyI() {
		eq(Signum2.POSITIVE.multiply( 72),  72);
		eq(Signum2.POSITIVE.multiply(-72), -72);
		eq(Signum2.NEGATIVE.multiply( 72), -72);
		eq(Signum2.NEGATIVE.multiply(-72),  72);
	}

	public void testMultiplyD() {
		eq(Signum2.POSITIVE.multiply( 72.0),  72.0);
		eq(Signum2.POSITIVE.multiply(-72.0), -72.0);
		eq(Signum2.NEGATIVE.multiply( 72.0), -72.0);
		eq(Signum2.NEGATIVE.multiply(-72.0),  72.0);
	}

	public void testNegate() {
		eq(Signum2.POSITIVE.negate(), Signum2.NEGATIVE);
		eq(Signum2.NEGATIVE.negate(), Signum2.POSITIVE);
	}

	public void testCompareTo() {
		eq(Signum2.POSITIVE.compareTo(Signum2.POSITIVE),  0);
		eq(Signum2.POSITIVE.compareTo(Signum2.NEGATIVE),  1);
		eq(Signum2.NEGATIVE.compareTo(Signum2.POSITIVE), -1);
		eq(Signum2.NEGATIVE.compareTo(Signum2.NEGATIVE),  0);
	}

}
