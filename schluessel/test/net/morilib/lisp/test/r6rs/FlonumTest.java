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
package net.morilib.lisp.test.r6rs;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/08/31
 */
public class FlonumTest extends TCSubr {

	public void testFlonumProps() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(flonum? 1.0)", T);
		eq   (l,"(flonum? 1)", F);
		eqr  (l,"(real->flonum 1.0)", 1.0);
		eqr  (l,"(real->flonum 1)", 1.0);
		eqr  (l,"(fixnum->flonum 1)", 1.0);
	}


	public void testFlonumProposition() {
		Scheme l = Scheme.newInstance();

		eq   (l,"(fl=? 1.0 1.0 1.0)", T);
		eq   (l,"(fl=? 1.0 2.0 3.0)", F);
		eq   (l,"(fl=? 1.0 1.0 3.0)", F);
		eq   (l,"(fl=? 3.0 2.0 1.0)", F);
		eq   (l,"(fl=? 3.0 1.0 1.0)", F);
		eq   (l,"(fl=? 1.0 3.0 2.0)", F);
		eq   (l,"(fl=? -inf.0 -inf.0)", T);
		eq   (l,"(fl=? -inf.0 +inf.0)", F);
		eq   (l,"(fl=? +inf.0 +inf.0)", T);
		eq   (l,"(fl=? +inf.0 -inf.0)", F);
		eq   (l,"(fl=? +nan.0 +nan.0)", F);
		eq   (l,"(fl=? 1.0 +nan.0)", F);

		eq   (l,"(fl<? 1.0 1.0 1.0)", F);
		eq   (l,"(fl<? 1.0 2.0 3.0)", T);
		eq   (l,"(fl<? 1.0 1.0 3.0)", F);
		eq   (l,"(fl<? 3.0 2.0 1.0)", F);
		eq   (l,"(fl<? 3.0 1.0 1.0)", F);
		eq   (l,"(fl<? 1.0 3.0 2.0)", F);
		eq   (l,"(fl<? -inf.0 -inf.0)", F);
		eq   (l,"(fl<? -inf.0 +inf.0)", T);
		eq   (l,"(fl<? +inf.0 +inf.0)", F);
		eq   (l,"(fl<? +inf.0 -inf.0)", F);
		eq   (l,"(fl<? +nan.0 +nan.0)", F);
		eq   (l,"(fl<? 1.0 +nan.0)", F);

		eq   (l,"(fl<=? 1.0 1.0 1.0)", T);
		eq   (l,"(fl<=? 1.0 2.0 3.0)", T);
		eq   (l,"(fl<=? 1.0 1.0 3.0)", T);
		eq   (l,"(fl<=? 3.0 2.0 1.0)", F);
		eq   (l,"(fl<=? 3.0 1.0 1.0)", F);
		eq   (l,"(fl<=? 1.0 3.0 2.0)", F);
		eq   (l,"(fl<=? -inf.0 -inf.0)", T);
		eq   (l,"(fl<=? -inf.0 +inf.0)", T);
		eq   (l,"(fl<=? +inf.0 +inf.0)", T);
		eq   (l,"(fl<=? +inf.0 -inf.0)", F);
		eq   (l,"(fl<=? +nan.0 +nan.0)", F);
		eq   (l,"(fl<=? 1.0 +nan.0)", F);

		eq   (l,"(fl>? 1.0 1.0 1.0)", F);
		eq   (l,"(fl>? 1.0 2.0 3.0)", F);
		eq   (l,"(fl>? 1.0 1.0 3.0)", F);
		eq   (l,"(fl>? 3.0 2.0 1.0)", T);
		eq   (l,"(fl>? 3.0 1.0 1.0)", F);
		eq   (l,"(fl>? 1.0 3.0 2.0)", F);
		eq   (l,"(fl>? -inf.0 -inf.0)", F);
		eq   (l,"(fl>? -inf.0 +inf.0)", F);
		eq   (l,"(fl>? +inf.0 +inf.0)", F);
		eq   (l,"(fl>? +inf.0 -inf.0)", T);
		eq   (l,"(fl>? +nan.0 +nan.0)", F);
		eq   (l,"(fl>? 1.0 +nan.0)", F);

		eq   (l,"(fl>=? 1.0 1.0 1.0)", T);
		eq   (l,"(fl>=? 1.0 2.0 3.0)", F);
		eq   (l,"(fl>=? 1.0 1.0 3.0)", F);
		eq   (l,"(fl>=? 3.0 2.0 1.0)", T);
		eq   (l,"(fl>=? 3.0 1.0 1.0)", T);
		eq   (l,"(fl>=? 1.0 3.0 2.0)", F);
		eq   (l,"(fl>=? -inf.0 -inf.0)", T);
		eq   (l,"(fl>=? -inf.0 +inf.0)", F);
		eq   (l,"(fl>=? +inf.0 +inf.0)", T);
		eq   (l,"(fl>=? +inf.0 -inf.0)", T);
		eq   (l,"(fl>=? +nan.0 +nan.0)", F);
		eq   (l,"(fl>=? 1.0 +nan.0)", F);

		eq   (l,"(flinteger?  0.0)", T);
		eq   (l,"(flinteger?  1.0)", T);
		eq   (l,"(flinteger? -1.0)", T);
		eq   (l,"(flinteger?  2.0)", T);
		eq   (l,"(flinteger? -2.0)", T);
		eq   (l,"(flinteger? -2.1)", F);
		eq   (l,"(flinteger? -0.0)", T);
		eq   (l,"(flinteger? -inf.0)", F);
		eq   (l,"(flinteger? +inf.0)", F);
		eq   (l,"(flinteger? +nan.0)", F);

		eq   (l,"(flzero?  0.0)", T);
		eq   (l,"(flzero?  1.0)", F);
		eq   (l,"(flzero? -1.0)", F);
		eq   (l,"(flzero?  2.0)", F);
		eq   (l,"(flzero? -2.0)", F);
		eq   (l,"(flzero? -0.0)", T);
		eq   (l,"(flzero? -inf.0)", F);
		eq   (l,"(flzero? +inf.0)", F);
		eq   (l,"(flzero? +nan.0)", F);

		eq   (l,"(flpositive?  0.0)", F);
		eq   (l,"(flpositive?  1.0)", T);
		eq   (l,"(flpositive? -1.0)", F);
		eq   (l,"(flpositive?  2.0)", T);
		eq   (l,"(flpositive? -2.0)", F);
		eq   (l,"(flpositive? -0.0)", F);
		eq   (l,"(flpositive? -inf.0)", F);
		eq   (l,"(flpositive? +inf.0)", T);
		eq   (l,"(flpositive? +nan.0)", F);

		eq   (l,"(flnegative?  0.0)", F);
		eq   (l,"(flnegative?  1.0)", F);
		eq   (l,"(flnegative? -1.0)", T);
		eq   (l,"(flnegative?  2.0)", F);
		eq   (l,"(flnegative? -2.0)", T);
		eq   (l,"(flnegative? -0.0)", F);
		eq   (l,"(flnegative? -inf.0)", T);
		eq   (l,"(flnegative? +inf.0)", F);
		eq   (l,"(flnegative? +nan.0)", F);

		eq   (l,"(flodd?  0.0)", F);
		eq   (l,"(flodd?  1.0)", T);
		eq   (l,"(flodd? -1.0)", T);
		eq   (l,"(flodd?  2.0)", F);
		eq   (l,"(flodd? -2.0)", F);
		eq   (l,"(flodd? -2.1)", F);
		eq   (l,"(flodd? -0.0)", F);
		eq   (l,"(flodd? -inf.0)", F);
		eq   (l,"(flodd? +inf.0)", F);
		eq   (l,"(flodd? +nan.0)", F);

		eq   (l,"(fleven?  0.0)", T);
		eq   (l,"(fleven?  1.0)", F);
		eq   (l,"(fleven? -1.0)", F);
		eq   (l,"(fleven?  2.0)", T);
		eq   (l,"(fleven? -2.0)", T);
		eq   (l,"(fleven? -2.1)", F);
		eq   (l,"(fleven? -0.0)", T);
		eq   (l,"(fleven? -inf.0)", F);
		eq   (l,"(fleven? +inf.0)", F);
		eq   (l,"(fleven? +nan.0)", F);

		eq   (l,"(flfinite?  0.0)", T);
		eq   (l,"(flfinite?  1.0)", T);
		eq   (l,"(flfinite? -1.0)", T);
		eq   (l,"(flfinite?  2.0)", T);
		eq   (l,"(flfinite? -2.0)", T);
		eq   (l,"(flfinite? -0.0)", T);
		eq   (l,"(flfinite? -inf.0)", F);
		eq   (l,"(flfinite? +inf.0)", F);
		eq   (l,"(flfinite? +nan.0)", F);

		eq   (l,"(flinfinite?  0.0)", F);
		eq   (l,"(flinfinite?  1.0)", F);
		eq   (l,"(flinfinite? -1.0)", F);
		eq   (l,"(flinfinite?  2.0)", F);
		eq   (l,"(flinfinite? -2.0)", F);
		eq   (l,"(flinfinite? -0.0)", F);
		eq   (l,"(flinfinite? -inf.0)", T);
		eq   (l,"(flinfinite? +inf.0)", T);
		eq   (l,"(flinfinite? +nan.0)", F);

		eq   (l,"(flnan?  0.0)", F);
		eq   (l,"(flnan?  1.0)", F);
		eq   (l,"(flnan? -1.0)", F);
		eq   (l,"(flnan?  2.0)", F);
		eq   (l,"(flnan? -2.0)", F);
		eq   (l,"(flnan? -0.0)", F);
		eq   (l,"(flnan? -inf.0)", F);
		eq   (l,"(flnan? +inf.0)", F);
		eq   (l,"(flnan? +nan.0)", T);
	}

	public void testFlExtrema() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(flmax 1.0 3.0 2.0 5.0 4.0)", 5.0);
		eqr  (l,"(flmax 1.0)", 1.0);
		eqr  (l,"(flmax 1.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flmax 1.0 -inf.0)", 1.0);
		eqr  (l,"(flmax +inf.0 1.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flmax -inf.0 1.0)", 1.0);
		eqr  (l,"(flmax +inf.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flmax -inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(flmax +inf.0 -inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flmax +inf.0 -inf.0)", Double.POSITIVE_INFINITY);
		isNaN(l,"(flmax 1.0 3.0 +nan.0 5.0)");

		eqr  (l,"(flmin 1.0 3.0 2.0 5.0 4.0)", 1.0);
		eqr  (l,"(flmin 1.0)", 1.0);
		eqr  (l,"(flmin 1.0 +inf.0)", 1.0);
		eqr  (l,"(flmin 1.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(flmin +inf.0 1.0)", 1.0);
		eqr  (l,"(flmin -inf.0 1.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(flmin +inf.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flmin -inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(flmin +inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(flmin +inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(flmin 1.0 3.0 +nan.0 5.0)");
	}

	public void testFxArithmeticOperation() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(fl+ 1.0 3.0 2.0 5.0 4.0)", 15.0);
		eqr  (l,"(fl+ 1.0)", 1.0);
		eqr  (l,"(fl+ 1.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl+ 1.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl+ +inf.0 1.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl+ -inf.0 1.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl+ +inf.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl+ -inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fl+ +inf.0 -inf.0)");
		isNaN(l,"(fl+ -inf.0 +inf.0)");
		isNaN(l,"(fl+ +nan.0 +inf.0)");
		isNaN(l,"(fl+ +nan.0 -inf.0)");
		isNaN(l,"(fl+ +inf.0 +nan.0)");
		isNaN(l,"(fl+ -inf.0 +nan.0)");
		isNaN(l,"(fl+ 1.0 3.0 +nan.0 5.0)");

		eqr  (l,"(fl- 1.0 3.0 2.0 5.0 4.0)", -13.0);
		eqr  (l,"(fl- 1.0)", -1.0);
		eqr  (l,"(fl- 1.0 +inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl- 1.0 -inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl- +inf.0 1.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl- -inf.0 1.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fl- +inf.0 +inf.0)");
		isNaN(l,"(fl- -inf.0 -inf.0)");
		eqr  (l,"(fl- +inf.0 -inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl- -inf.0 +inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fl- +nan.0 +inf.0)");
		isNaN(l,"(fl- +nan.0 -inf.0)");
		isNaN(l,"(fl- +inf.0 +nan.0)");
		isNaN(l,"(fl- -inf.0 +nan.0)");
		isNaN(l,"(fl- 1.0 3.0 +nan.0 5.0)");

		eqr  (l,"(fl* 1.0 3.0 2.0 5.0 4.0)", 120.0);
		eqr  (l,"(fl* 1.0)", 1.0);
		eqr  (l,"(fl* 1.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl* 1.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl* +inf.0 1.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl* -inf.0 1.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl* +inf.0 +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl* -inf.0 -inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl* +inf.0 -inf.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl* -inf.0 +inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fl* +nan.0 +inf.0)");
		isNaN(l,"(fl* +nan.0 -inf.0)");
		isNaN(l,"(fl* +inf.0 +nan.0)");
		isNaN(l,"(fl* -inf.0 +nan.0)");
		isNaN(l,"(fl* 1.0 3.0 +nan.0 5.0)");

		eqr  (l,"(fl/ 1.0 4.0 2.0)", 0.125);
		eqr  (l,"(fl/ 2.0)", 0.5);
		eqr  (l,"(fl/ 1.0 +inf.0)", 0.0);
		eqr  (l,"(fl/ 1.0 -inf.0)", -0.0);
		eqr  (l,"(fl/ +inf.0 1.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl/ -inf.0 1.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fl/ +inf.0 +inf.0)");
		isNaN(l,"(fl/ -inf.0 -inf.0)");
		isNaN(l,"(fl/ +inf.0 -inf.0)");
		isNaN(l,"(fl/ -inf.0 +inf.0)");
		eqr  (l,"(fl/  1.0  0.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fl/  1.0 -0.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl/ -1.0  0.0)", Double.NEGATIVE_INFINITY);
		eqr  (l,"(fl/ -1.0 -0.0)", Double.POSITIVE_INFINITY);
		isNaN(l,"(fl/  0.0  0.0)");
		isNaN(l,"(fl/ +nan.0 +inf.0)");
		isNaN(l,"(fl/ +nan.0 -inf.0)");
		isNaN(l,"(fl/ +inf.0 +nan.0)");
		isNaN(l,"(fl/ -inf.0 +nan.0)");
		isNaN(l,"(fl/ 1.0 3.0 +nan.0 5.0)");
	}

	public void testFxAbs() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(flabs 72.0)", 72.0);
		eqr  (l,"(flabs -72.0)", 72.0);
		eqr  (l,"(flabs +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flabs -inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flabs 0.0)", 0.0);
		eqr  (l,"(flabs -0.0)", 0.0);
		isNaN(l,"(flabs +nan.0)");
	}

	public void testFlDivMod() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(fldiv  123.0  10.0)", 12.0);
		eqr  (l,"(flmod  123.0  10.0)", 3.0);
		eqr  (l,"(fldiv  123.0 -10.0)", -12.0);
		eqr  (l,"(flmod  123.0 -10.0)", 3.0);
		eqr  (l,"(fldiv -123.0  10.0)", -13.0);
		eqr  (l,"(flmod -123.0  10.0)", 7.0);
		eqr  (l,"(fldiv -123.0 -10.0)", 13.0);
		eqr  (l,"(flmod -123.0 -10.0)", 7.0);
		eqr  (l,"(fldiv-and-mod  123.0  10.0)", 12.0, 3.0);
		eqr  (l,"(fldiv-and-mod  123.0 -10.0)", -12.0, 3.0);
		eqr  (l,"(fldiv-and-mod -123.0  10.0)", -13.0, 7.0);
		eqr  (l,"(fldiv-and-mod -123.0 -10.0)", 13.0, 7.0);

		eqr  (l,"(fldiv0  123.0  10.0)", 12.0);
		eqr  (l,"(flmod0  123.0  10.0)", 3.0);
		eqr  (l,"(fldiv0  123.0 -10.0)", -12.0);
		eqr  (l,"(flmod0  123.0 -10.0)", 3.0);
		eqr  (l,"(fldiv0 -123.0  10.0)", -12.0);
		eqr  (l,"(flmod0 -123.0  10.0)", -3.0);
		eqr  (l,"(fldiv0 -123.0 -10.0)", 12.0);
		eqr  (l,"(flmod0 -123.0 -10.0)", -3.0);
		eqr  (l,"(fldiv0-and-mod0  123.0  10.0)", 12.0, 3.0);
		eqr  (l,"(fldiv0-and-mod0  123.0 -10.0)", -12.0, 3.0);
		eqr  (l,"(fldiv0-and-mod0 -123.0  10.0)", -12.0, -3.0);
		eqr  (l,"(fldiv0-and-mod0 -123.0 -10.0)", 12.0, -3.0);

		isNaN(l,"(fldiv 1.0 0.0)");
		isNaN(l,"(flmod 1.0 0.0)");
		eqr  (l,"(fldiv-and-mod 1.0 0.0)", Double.NaN, Double.NaN);
		isNaN(l,"(fldiv0 1.0 0.0)");
		isNaN(l,"(flmod0 1.0 0.0)");
		eqr  (l,"(fldiv0-and-mod0 1.0 0.0)", Double.NaN, Double.NaN);
	}

	public void testFlRational() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(flnumerator 0.75)", 3.0);
		eqr  (l,"(flnumerator -0.75)", -3.0);
		eqr  (l,"(flnumerator 72.0)", 72.0);
		eqr  (l,"(flnumerator 0.0)", 0.0);
		eqr  (l,"(flnumerator -0.0)", -0.0);
		eqr  (l,"(flnumerator +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flnumerator -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(flnumerator +nan.0)");

		eqr  (l,"(fldenominator 0.75)", 4.0);
		eqr  (l,"(fldenominator -0.75)", 4.0);
		eqr  (l,"(fldenominator 72.0)", 1.0);
		eqr  (l,"(fldenominator -0.0)", 1.0);
		eqr  (l,"(fldenominator 0.0)", 1.0);
		eqr  (l,"(fldenominator +inf.0)", 1.0);
		eqr  (l,"(fldenominator -inf.0)", 1.0);
		isNaN(l,"(fldenominator +nan.0)");
	}

	public void testFlRound() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(flfloor 7.2)", 7.0);
		eqr  (l,"(flfloor -7.2)", -8.0);
		eqr  (l,"(flfloor 7.7)", 7.0);
		eqr  (l,"(flfloor -7.7)", -8.0);
		eqr  (l,"(flfloor 9.0)", 9.0);
		eqr  (l,"(flfloor -9.0)", -9.0);
		eqr  (l,"(flfloor +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flfloor -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(flfloor +nan.0)");

		eqr  (l,"(flceiling 7.2)", 8.0);
		eqr  (l,"(flceiling -7.2)", -7.0);
		eqr  (l,"(flceiling 7.7)", 8.0);
		eqr  (l,"(flceiling -7.7)", -7.0);
		eqr  (l,"(flceiling 9.0)", 9.0);
		eqr  (l,"(flceiling -9.0)", -9.0);
		eqr  (l,"(flceiling +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flceiling -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(flceiling +nan.0)");

		eqr  (l,"(fltruncate 7.2)", 7.0);
		eqr  (l,"(fltruncate -7.2)", -7.0);
		eqr  (l,"(fltruncate 7.7)", 7.0);
		eqr  (l,"(fltruncate -7.7)", -7.0);
		eqr  (l,"(fltruncate 9.0)", 9.0);
		eqr  (l,"(fltruncate -9.0)", -9.0);
		eqr  (l,"(fltruncate +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(fltruncate -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(fltruncate +nan.0)");

		eqr  (l,"(flround 7.2)", 7.0);
		eqr  (l,"(flround -7.2)", -7.0);
		eqr  (l,"(flround 7.7)", 8.0);
		eqr  (l,"(flround -7.7)", -8.0);
		eqr  (l,"(flround 9.0)", 9.0);
		eqr  (l,"(flround -9.0)", -9.0);
		eqr  (l,"(flround +inf.0)", Double.POSITIVE_INFINITY);
		eqr  (l,"(flround -inf.0)", Double.NEGATIVE_INFINITY);
		isNaN(l,"(flround +nan.0)");
	}

	public void testFlElementaryMath() {
		Scheme l = Scheme.newInstance();

		eqr  (l,"(flexp 0.0)", 1.0);
		eqr  (l,"(fllog 1.0)", 0);
		eqr  (l,"(flround (fllog 8.0 2.0))", 3.0);
		eqr  (l,"(flsin 0.0)", 0.0);
		eqr  (l,"(flcos 0.0)", 1.0);

		eqr  (l,"(flsqrt 841.0)", 29.0);
		eqr  (l,"(flsqrt 0.0)", 0.0);
		eqr  (l,"(flsqrt -0.0)", -0.0);

		eqr  (l,"(flexpt 2.0 3.0)", 8.0);
	}

}
