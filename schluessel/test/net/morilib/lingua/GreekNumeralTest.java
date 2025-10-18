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
package net.morilib.lingua;

import net.morilib.lingua.numeral.IonianNumeral;
import net.morilib.lisp.test.TC;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/05/10
 */
public class GreekNumeralTest extends TC {

	public void testToGreek() {
		eq(IonianNumeral.toGreek(   1), "α'");
		eq(IonianNumeral.toGreek(   2), "β'");
		eq(IonianNumeral.toGreek(   3), "γ'");
		eq(IonianNumeral.toGreek(   4), "δ'");
		eq(IonianNumeral.toGreek(   5), "ε'");
		eq(IonianNumeral.toGreek(   6), "\u03db'");
		eq(IonianNumeral.toGreek(   7), "ζ'");
		eq(IonianNumeral.toGreek(   8), "η'");
		eq(IonianNumeral.toGreek(   9), "θ'");
		eq(IonianNumeral.toGreek(  10), "ι'");
		eq(IonianNumeral.toGreek(  20), "κ'");
		eq(IonianNumeral.toGreek(  30), "λ'");
		eq(IonianNumeral.toGreek(  40), "μ'");
		eq(IonianNumeral.toGreek(  50), "ν'");
		eq(IonianNumeral.toGreek(  60), "ξ'");
		eq(IonianNumeral.toGreek(  70), "ο'");
		eq(IonianNumeral.toGreek(  80), "π'");
		eq(IonianNumeral.toGreek(  90), "\u03d9'");
		eq(IonianNumeral.toGreek( 100), "ρ'");
		eq(IonianNumeral.toGreek( 200), "σ'");
		eq(IonianNumeral.toGreek( 300), "τ'");
		eq(IonianNumeral.toGreek( 400), "υ'");
		eq(IonianNumeral.toGreek( 500), "φ'");
		eq(IonianNumeral.toGreek( 600), "χ'");
		eq(IonianNumeral.toGreek( 700), "ψ'");
		eq(IonianNumeral.toGreek( 800), "ω'");
		eq(IonianNumeral.toGreek( 900), "\u03e1'");
		eq(IonianNumeral.toGreek(1000), ",α'");
		eq(IonianNumeral.toGreek(2000), ",β'");
		eq(IonianNumeral.toGreek(3000), ",γ'");
		eq(IonianNumeral.toGreek(4000), ",δ'");
		eq(IonianNumeral.toGreek(5000), ",ε'");
		eq(IonianNumeral.toGreek(6000), ",\u03db'");
		eq(IonianNumeral.toGreek(7000), ",ζ'");
		eq(IonianNumeral.toGreek(8000), ",η'");
		eq(IonianNumeral.toGreek(9000), ",θ'");
		eq(IonianNumeral.toGreek(  72), "οβ'");
		eq(IonianNumeral.toGreek( 765), "ψξε'");
		eq(IonianNumeral.toGreek(1729), ",αψκθ'");
		nil(IonianNumeral.toGreek(0));
		nil(IonianNumeral.toGreek(10000));
	}

	public void testParse() {
		eq(IonianNumeral.parse("α'"),    1);
		eq(IonianNumeral.parse("β'"),    2);
		eq(IonianNumeral.parse("γ'"),    3);
		eq(IonianNumeral.parse("δ'"),    4);
		eq(IonianNumeral.parse("ε'"),    5);
		eq(IonianNumeral.parse("\u03db'"),    6);
		eq(IonianNumeral.parse("ζ'"),    7);
		eq(IonianNumeral.parse("η'"),    8);
		eq(IonianNumeral.parse("θ'"),    9);
		eq(IonianNumeral.parse("ι'"),   10);
		eq(IonianNumeral.parse("κ'"),   20);
		eq(IonianNumeral.parse("λ'"),   30);
		eq(IonianNumeral.parse("μ'"),   40);
		eq(IonianNumeral.parse("ν'"),   50);
		eq(IonianNumeral.parse("ξ'"),   60);
		eq(IonianNumeral.parse("ο'"),   70);
		eq(IonianNumeral.parse("π'"),   80);
		eq(IonianNumeral.parse("\u03d9'"),   90);
		eq(IonianNumeral.parse("ρ'"),  100);
		eq(IonianNumeral.parse("σ'"),  200);
		eq(IonianNumeral.parse("τ'"),  300);
		eq(IonianNumeral.parse("υ'"),  400);
		eq(IonianNumeral.parse("φ'"),  500);
		eq(IonianNumeral.parse("χ'"),  600);
		eq(IonianNumeral.parse("ψ'"),  700);
		eq(IonianNumeral.parse("ω'"),  800);
		eq(IonianNumeral.parse("\u03e1'"),  900);
		eq(IonianNumeral.parse(",α'"), 1000);
		eq(IonianNumeral.parse(",β'"), 2000);
		eq(IonianNumeral.parse(",γ'"), 3000);
		eq(IonianNumeral.parse(",δ'"), 4000);
		eq(IonianNumeral.parse(",ε'"), 5000);
		eq(IonianNumeral.parse(",\u03db'"), 6000);
		eq(IonianNumeral.parse(",ζ'"), 7000);
		eq(IonianNumeral.parse(",η'"), 8000);
		eq(IonianNumeral.parse(",θ'"), 9000);
		eq(IonianNumeral.parse("οβ'"),   72);
		eq(IonianNumeral.parse("ψξε'"),  765);
		eq(IonianNumeral.parse(",αψκθ'"), 1729);
		eq(IonianNumeral.parse(",αα'"), 1001);
		eq(IonianNumeral.parse(""), -1);
		eq(IonianNumeral.parse("αψκθ'"), -1);
		eq(IonianNumeral.parse("αα'"), -1);
		eq(IonianNumeral.parse(",ο"), -1);
		eq(IonianNumeral.parse("III"), -1);
	}

}
