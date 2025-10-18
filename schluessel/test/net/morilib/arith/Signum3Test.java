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
public class Signum3Test extends TC {

	public void testToSignum() {
		eq(Signum3.toSignum( 1), Signum3.POSITIVE);
		eq(Signum3.toSignum( 0), Signum3.ZERO);
		eq(Signum3.toSignum(-1), Signum3.NEGATIVE);

		eq(Signum3.toSignum( 1l), Signum3.POSITIVE);
		eq(Signum3.toSignum( 0l), Signum3.ZERO);
		eq(Signum3.toSignum(-1l), Signum3.NEGATIVE);
	}

	public void testMultiplyS() {
		eq(Signum3.POSITIVE.multiply(Signum3.POSITIVE), Signum3.POSITIVE);
		eq(Signum3.POSITIVE.multiply(Signum3.ZERO),     Signum3.ZERO);
		eq(Signum3.POSITIVE.multiply(Signum3.NEGATIVE), Signum3.NEGATIVE);
		eq(Signum3.ZERO.multiply(Signum3.POSITIVE),     Signum3.ZERO);
		eq(Signum3.ZERO.multiply(Signum3.ZERO),         Signum3.ZERO);
		eq(Signum3.ZERO.multiply(Signum3.NEGATIVE),     Signum3.ZERO);
		eq(Signum3.NEGATIVE.multiply(Signum3.POSITIVE), Signum3.NEGATIVE);
		eq(Signum3.NEGATIVE.multiply(Signum3.ZERO),     Signum3.ZERO);
		eq(Signum3.NEGATIVE.multiply(Signum3.NEGATIVE), Signum3.POSITIVE);
	}

	public void testSignum() {
		eq(Signum3.POSITIVE.signum(),  1);
		eq(Signum3.ZERO.signum()    ,  0);
		eq(Signum3.NEGATIVE.signum(), -1);
	}

	public void testMultiplyI() {
		eq(Signum3.POSITIVE.multiply( 72),  72);
		eq(Signum3.POSITIVE.multiply(-72), -72);
		eq(Signum3.ZERO.multiply( 72),       0);
		eq(Signum3.ZERO.multiply(-72),       0);
		eq(Signum3.NEGATIVE.multiply( 72), -72);
		eq(Signum3.NEGATIVE.multiply(-72),  72);
	}

	public void testMultiplyD() {
		eq(Signum3.POSITIVE.multiply( 72.0),  72.0);
		eq(Signum3.POSITIVE.multiply(-72.0), -72.0);
		eq(Signum3.ZERO.multiply( 72.0),       0.0);
		eq(Signum3.ZERO.multiply(-72.0),       0.0);
		eq(Signum3.NEGATIVE.multiply( 72.0), -72.0);
		eq(Signum3.NEGATIVE.multiply(-72.0),  72.0);
	}

	public void testNegate() {
		eq(Signum3.POSITIVE.negate(), Signum3.NEGATIVE);
		eq(Signum3.ZERO.negate(),     Signum3.ZERO);
		eq(Signum3.NEGATIVE.negate(), Signum3.POSITIVE);
	}

	public void testCompareTo() {
		eq(Signum3.POSITIVE.compareTo(Signum3.POSITIVE),  0);
		eq(Signum3.POSITIVE.compareTo(Signum3.ZERO),      1);
		eq(Signum3.POSITIVE.compareTo(Signum3.NEGATIVE),  1);
		eq(Signum3.ZERO.compareTo(Signum3.POSITIVE),     -1);
		eq(Signum3.ZERO.compareTo(Signum3.ZERO),          0);
		eq(Signum3.ZERO.compareTo(Signum3.NEGATIVE),      1);
		eq(Signum3.NEGATIVE.compareTo(Signum3.POSITIVE), -1);
		eq(Signum3.NEGATIVE.compareTo(Signum3.ZERO),     -1);
		eq(Signum3.NEGATIVE.compareTo(Signum3.NEGATIVE),  0);
	}

}
