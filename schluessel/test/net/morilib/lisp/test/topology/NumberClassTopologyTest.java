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
package net.morilib.lisp.test.topology;

import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.topology.LispCardinality;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/11/12
 */
public class NumberClassTopologyTest extends TCSubr {

	public void testIsNeighborhoodOf() {
		Scheme l = Scheme.newInstance();

		equal(l, "(neighborhood-of? *O* 1+2jo)", T);
		equal(l, "(neighborhood-of? *O* 1+2j)",  T);
		equal(l, "(neighborhood-of? *O* 1+2i)",  T);
		equal(l, "(neighborhood-of? *O* -1/2)",   T);
		equal(l, "(neighborhood-of? *O* #i1/3)",  T);
		equal(l, "(neighborhood-of? *O* -1)",     T);
		equal(l, "(neighborhood-of? *O* 0)",      T);
		equal(l, "(neighborhood-of? *O* 1)",      T);

		equal(l, "(neighborhood-of? *H* 1+2jo)", F);
		equal(l, "(neighborhood-of? *H* 1+2j)",  T);
		equal(l, "(neighborhood-of? *H* 1+2i)",  T);
		equal(l, "(neighborhood-of? *H* -1/2)",  T);
		equal(l, "(neighborhood-of? *H* #i1/3)", T);
		equal(l, "(neighborhood-of? *H* -1)",    T);
		equal(l, "(neighborhood-of? *H* 0)",     T);
		equal(l, "(neighborhood-of? *H* 1)",     T);

		equal(l, "(neighborhood-of? *C* 1+2jo)", F);
		equal(l, "(neighborhood-of? *C* 1+2j)",  F);
		equal(l, "(neighborhood-of? *C* 1+2i)",  T);
		equal(l, "(neighborhood-of? *C* -1/2)",  T);
		equal(l, "(neighborhood-of? *C* #i1/3)", T);
		equal(l, "(neighborhood-of? *C* -1)",    T);
		equal(l, "(neighborhood-of? *C* 0)",     T);
		equal(l, "(neighborhood-of? *C* 1)",     T);

		equal(l, "(neighborhood-of? *R* 1+2jo)", F);
		equal(l, "(neighborhood-of? *R* 1+2j)",  F);
		equal(l, "(neighborhood-of? *R* 1+2i)",  F);
		equal(l, "(neighborhood-of? *R* -1/2)",  T);
		equal(l, "(neighborhood-of? *R* #i1/3)", T);
		equal(l, "(neighborhood-of? *R* -1)",    T);
		equal(l, "(neighborhood-of? *R* 0)",     T);
		equal(l, "(neighborhood-of? *R* 1)",     T);

		equal(l, "(neighborhood-of? *Q* 1+2jo)", F);
		equal(l, "(neighborhood-of? *Q* 1+2j)",  F);
		equal(l, "(neighborhood-of? *Q* 1+2i)",  F);
		equal(l, "(neighborhood-of? *Q* -1/2)",  T);
		equal(l, "(neighborhood-of? *Q* #i1/3)", T);
		equal(l, "(neighborhood-of? *Q* -1)",    T);
		equal(l, "(neighborhood-of? *Q* 0)",     T);
		equal(l, "(neighborhood-of? *Q* 1)",     T);

		equal(l, "(neighborhood-of? *Z* 1+2jo)", F);
		equal(l, "(neighborhood-of? *Z* 1+2j)",  F);
		equal(l, "(neighborhood-of? *Z* 1+2i)",  F);
		equal(l, "(neighborhood-of? *Z* -1/2)",  F);
		equal(l, "(neighborhood-of? *Z* #i1/3)", F);
		equal(l, "(neighborhood-of? *Z* -1)",    T);
		equal(l, "(neighborhood-of? *Z* 0)",     T);
		equal(l, "(neighborhood-of? *Z* 1)",     T);

		equal(l, "(neighborhood-of? *N* 1+2jo)", F);
		equal(l, "(neighborhood-of? *N* 1+2j)",  F);
		equal(l, "(neighborhood-of? *N* 1+2i)",  F);
		equal(l, "(neighborhood-of? *N* -1/2)",  F);
		equal(l, "(neighborhood-of? *N* #i1/3)", F);
		equal(l, "(neighborhood-of? *N* -1)",    F);
		equal(l, "(neighborhood-of? *N* 0)",     T);
		equal(l, "(neighborhood-of? *N* 1)",     T);

		equal(l, "(neighborhood-of? *N+* +1+2jo)", F);
		equal(l, "(neighborhood-of? *N+* +1+2j)",  F);
		equal(l, "(neighborhood-of? *N+* +1+2i)",  F);
		equal(l, "(neighborhood-of? *N+* -1/2)",   F);
		equal(l, "(neighborhood-of? *N+* #i1/3)",  F);
		equal(l, "(neighborhood-of? *N+* -1)",     F);
		equal(l, "(neighborhood-of? *N+* 0)",      F);
		equal(l, "(neighborhood-of? *N+* 1)",      T);
	}

	public void testIsContained() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-contained? *O* *O*)",  T);
		equal(l, "(topology-contained? *O* *H*)",  F);
		equal(l, "(topology-contained? *O* *C*)",  F);
		equal(l, "(topology-contained? *O* *R*)",  F);
		equal(l, "(topology-contained? *O* *Q*)",  F);
		equal(l, "(topology-contained? *O* *Z*)",  F);
		equal(l, "(topology-contained? *O* *N*)",  F);
		equal(l, "(topology-contained? *O* *N+*)", F);

		equal(l, "(topology-contained? *H* *O*)",  T);
		equal(l, "(topology-contained? *H* *H*)",  T);
		equal(l, "(topology-contained? *H* *C*)",  F);
		equal(l, "(topology-contained? *H* *R*)",  F);
		equal(l, "(topology-contained? *H* *Q*)",  F);
		equal(l, "(topology-contained? *H* *Z*)",  F);
		equal(l, "(topology-contained? *H* *N*)",  F);
		equal(l, "(topology-contained? *H* *N+*)", F);

		equal(l, "(topology-contained? *C* *O*)",  T);
		equal(l, "(topology-contained? *C* *H*)",  T);
		equal(l, "(topology-contained? *C* *C*)",  T);
		equal(l, "(topology-contained? *C* *R*)",  F);
		equal(l, "(topology-contained? *C* *Q*)",  F);
		equal(l, "(topology-contained? *C* *Z*)",  F);
		equal(l, "(topology-contained? *C* *N*)",  F);
		equal(l, "(topology-contained? *C* *N+*)", F);

		equal(l, "(topology-contained? *R* *O*)",  T);
		equal(l, "(topology-contained? *R* *H*)",  T);
		equal(l, "(topology-contained? *R* *C*)",  T);
		equal(l, "(topology-contained? *R* *R*)",  T);
		equal(l, "(topology-contained? *R* *Q*)",  F);
		equal(l, "(topology-contained? *R* *Z*)",  F);
		equal(l, "(topology-contained? *R* *N*)",  F);
		equal(l, "(topology-contained? *R* *N+*)", F);

		equal(l, "(topology-contained? *Q* *O*)",  T);
		equal(l, "(topology-contained? *Q* *H*)",  T);
		equal(l, "(topology-contained? *Q* *C*)",  T);
		equal(l, "(topology-contained? *Q* *R*)",  T);
		equal(l, "(topology-contained? *Q* *Q*)",  T);
		equal(l, "(topology-contained? *Q* *Z*)",  F);
		equal(l, "(topology-contained? *Q* *N*)",  F);
		equal(l, "(topology-contained? *Q* *N+*)", F);

		equal(l, "(topology-contained? *Z* *O*)",  T);
		equal(l, "(topology-contained? *Z* *H*)",  T);
		equal(l, "(topology-contained? *Z* *C*)",  T);
		equal(l, "(topology-contained? *Z* *R*)",  T);
		equal(l, "(topology-contained? *Z* *Q*)",  T);
		equal(l, "(topology-contained? *Z* *Z*)",  T);
		equal(l, "(topology-contained? *Z* *N*)",  F);
		equal(l, "(topology-contained? *Z* *N+*)", F);

		equal(l, "(topology-contained? *N* *O*)",  T);
		equal(l, "(topology-contained? *N* *H*)",  T);
		equal(l, "(topology-contained? *N* *C*)",  T);
		equal(l, "(topology-contained? *N* *R*)",  T);
		equal(l, "(topology-contained? *N* *Q*)",  T);
		equal(l, "(topology-contained? *N* *Z*)",  T);
		equal(l, "(topology-contained? *N* *N*)",  T);
		equal(l, "(topology-contained? *N* *N+*)", F);

		equal(l, "(topology-contained? *N+* *O*)",  T);
		equal(l, "(topology-contained? *N+* *H*)",  T);
		equal(l, "(topology-contained? *N+* *C*)",  T);
		equal(l, "(topology-contained? *N+* *R*)",  T);
		equal(l, "(topology-contained? *N+* *Q*)",  T);
		equal(l, "(topology-contained? *N+* *Z*)",  T);
		equal(l, "(topology-contained? *N+* *N*)",  T);
		equal(l, "(topology-contained? *N+* *N+*)", T);

		equal(l, "(topology-contained? *O*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-contained? *H*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-contained? *C*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-contained? *R*  #R(-inf.0 +inf.0))", T);
		equal(l, "(topology-contained? *Q*  #R(-inf.0 +inf.0))", T);
		equal(l, "(topology-contained? *Z*  #R(-inf.0 +inf.0))", T);
		equal(l, "(topology-contained? *N*  #R(-inf.0 +inf.0))", T);
		equal(l, "(topology-contained? *N+* #R(-inf.0 +inf.0))", T);

		equal(l, "(topology-contained? *O*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *H*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *C*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *R*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *Q*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *Z*  #R[0 +inf.0))", F);
		equal(l, "(topology-contained? *N*  #R[0 +inf.0))", T);
		equal(l, "(topology-contained? *N+* #R[0 +inf.0))", T);

		equal(l, "(topology-contained? *O*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *H*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *C*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *R*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *Q*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *Z*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *N*  #R[1 +inf.0))", F);
		equal(l, "(topology-contained? *N+* #R[1 +inf.0))", T);

		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *O*)",  T);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *H*)",  T);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *C*)",  T);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *R*)",  T);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *Q*)",  F);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *Z*)",  F);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *N*)",  F);
		equal(l, "(topology-contained? #R(-inf.0 +inf.0) *N+*)", F);

		equal(l, "(topology-contained? #R[1/2 1/2] *O*)",  T);
		equal(l, "(topology-contained? #R[1/2 1/2] *H*)",  T);
		equal(l, "(topology-contained? #R[1/2 1/2] *C*)",  T);
		equal(l, "(topology-contained? #R[1/2 1/2] *R*)",  T);
		equal(l, "(topology-contained? #R[1/2 1/2] *Q*)",  T);
		equal(l, "(topology-contained? #R[1/2 1/2] *Z*)",  F);
		equal(l, "(topology-contained? #R[1/2 1/2] *N*)",  F);
		equal(l, "(topology-contained? #R[1/2 1/2] *N+*)", F);

		equal(l, "(topology-contained? #R[-1 -1] *O*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *H*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *C*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *R*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *Q*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *Z*)",  T);
		equal(l, "(topology-contained? #R[-1 -1] *N*)",  F);
		equal(l, "(topology-contained? #R[-1 -1] *N+*)", F);

		equal(l, "(topology-contained? #R[0 0] *O*)",  T);
		equal(l, "(topology-contained? #R[0 0] *H*)",  T);
		equal(l, "(topology-contained? #R[0 0] *C*)",  T);
		equal(l, "(topology-contained? #R[0 0] *R*)",  T);
		equal(l, "(topology-contained? #R[0 0] *Q*)",  T);
		equal(l, "(topology-contained? #R[0 0] *Z*)",  T);
		equal(l, "(topology-contained? #R[0 0] *N*)",  T);
		equal(l, "(topology-contained? #R[0 0] *N+*)", F);

		equal(l, "(topology-contained? #R[1 1] *O*)",  T);
		equal(l, "(topology-contained? #R[1 1] *H*)",  T);
		equal(l, "(topology-contained? #R[1 1] *C*)",  T);
		equal(l, "(topology-contained? #R[1 1] *R*)",  T);
		equal(l, "(topology-contained? #R[1 1] *Q*)",  T);
		equal(l, "(topology-contained? #R[1 1] *Z*)",  T);
		equal(l, "(topology-contained? #R[1 1] *N*)",  T);
		equal(l, "(topology-contained? #R[1 1] *N+*)", T);
	}

	public void testTopologyCardinality() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-cardinality *O*)",  LispCardinality.C);
		equal(l, "(topology-cardinality *H*)",  LispCardinality.C);
		equal(l, "(topology-cardinality *C*)",  LispCardinality.C);
		equal(l, "(topology-cardinality *R*)",  LispCardinality.C);
		equal(l, "(topology-cardinality *Q*)",  LispCardinality.A);
		equal(l, "(topology-cardinality *Z*)",  LispCardinality.A);
		equal(l, "(topology-cardinality *N*)",  LispCardinality.A);
		equal(l, "(topology-cardinality *N+*)", LispCardinality.A);
	}

	public void testIsIndependent() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-independent? *O* *O*)",  F);
		equal(l, "(topology-independent? *O* *H*)",  F);
		equal(l, "(topology-independent? *O* *C*)",  F);
		equal(l, "(topology-independent? *O* *R*)",  F);
		equal(l, "(topology-independent? *O* *Q*)",  F);
		equal(l, "(topology-independent? *O* *Z*)",  F);
		equal(l, "(topology-independent? *O* *N*)",  F);
		equal(l, "(topology-independent? *O* *N+*)", F);

		equal(l, "(topology-independent? *H* *O*)",  F);
		equal(l, "(topology-independent? *H* *H*)",  F);
		equal(l, "(topology-independent? *H* *C*)",  F);
		equal(l, "(topology-independent? *H* *R*)",  F);
		equal(l, "(topology-independent? *H* *Q*)",  F);
		equal(l, "(topology-independent? *H* *Z*)",  F);
		equal(l, "(topology-independent? *H* *N*)",  F);
		equal(l, "(topology-independent? *H* *N+*)", F);

		equal(l, "(topology-independent? *C* *O*)",  F);
		equal(l, "(topology-independent? *C* *H*)",  F);
		equal(l, "(topology-independent? *C* *C*)",  F);
		equal(l, "(topology-independent? *C* *R*)",  F);
		equal(l, "(topology-independent? *C* *Q*)",  F);
		equal(l, "(topology-independent? *C* *Z*)",  F);
		equal(l, "(topology-independent? *C* *N*)",  F);
		equal(l, "(topology-independent? *C* *N+*)", F);

		equal(l, "(topology-independent? *R* *O*)",  F);
		equal(l, "(topology-independent? *R* *H*)",  F);
		equal(l, "(topology-independent? *R* *C*)",  F);
		equal(l, "(topology-independent? *R* *R*)",  F);
		equal(l, "(topology-independent? *R* *Q*)",  F);
		equal(l, "(topology-independent? *R* *Z*)",  F);
		equal(l, "(topology-independent? *R* *N*)",  F);
		equal(l, "(topology-independent? *R* *N+*)", F);

		equal(l, "(topology-independent? *Q* *O*)",  F);
		equal(l, "(topology-independent? *Q* *H*)",  F);
		equal(l, "(topology-independent? *Q* *C*)",  F);
		equal(l, "(topology-independent? *Q* *R*)",  F);
		equal(l, "(topology-independent? *Q* *Q*)",  F);
		equal(l, "(topology-independent? *Q* *Z*)",  F);
		equal(l, "(topology-independent? *Q* *N*)",  F);
		equal(l, "(topology-independent? *Q* *N+*)", F);

		equal(l, "(topology-independent? *Z* *O*)",  F);
		equal(l, "(topology-independent? *Z* *H*)",  F);
		equal(l, "(topology-independent? *Z* *C*)",  F);
		equal(l, "(topology-independent? *Z* *R*)",  F);
		equal(l, "(topology-independent? *Z* *Q*)",  F);
		equal(l, "(topology-independent? *Z* *Z*)",  F);
		equal(l, "(topology-independent? *Z* *N*)",  F);
		equal(l, "(topology-independent? *Z* *N+*)", F);

		equal(l, "(topology-independent? *N* *O*)",  F);
		equal(l, "(topology-independent? *N* *H*)",  F);
		equal(l, "(topology-independent? *N* *C*)",  F);
		equal(l, "(topology-independent? *N* *R*)",  F);
		equal(l, "(topology-independent? *N* *Q*)",  F);
		equal(l, "(topology-independent? *N* *Z*)",  F);
		equal(l, "(topology-independent? *N* *N*)",  F);
		equal(l, "(topology-independent? *N* *N+*)", F);

		equal(l, "(topology-independent? *N+* *O*)",  F);
		equal(l, "(topology-independent? *N+* *H*)",  F);
		equal(l, "(topology-independent? *N+* *C*)",  F);
		equal(l, "(topology-independent? *N+* *R*)",  F);
		equal(l, "(topology-independent? *N+* *Q*)",  F);
		equal(l, "(topology-independent? *N+* *Z*)",  F);
		equal(l, "(topology-independent? *N+* *N*)",  F);
		equal(l, "(topology-independent? *N+* *N+*)", F);

		equal(l, "(topology-independent? *O*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *H*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *C*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *R*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *Q*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *Z*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *N*  #R(-inf.0 +inf.0))", F);
		equal(l, "(topology-independent? *N+* #R(-inf.0 +inf.0))", F);

		equal(l, "(topology-independent? *O*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *H*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *C*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *R*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *Q*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *Z*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *N*  #R(-inf.0 1))", F);
		equal(l, "(topology-independent? *N+* #R(-inf.0 1))", T);

		equal(l, "(topology-independent? *O*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *H*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *C*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *R*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *Q*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *Z*  #R(-inf.0 0))", F);
		equal(l, "(topology-independent? *N*  #R(-inf.0 0))", T);
		equal(l, "(topology-independent? *N+* #R(-inf.0 0))", T);

		equal(l, "(topology-independent? *O*  #R(2 3))", F);
		equal(l, "(topology-independent? *H*  #R(2 3))", F);
		equal(l, "(topology-independent? *C*  #R(2 3))", F);
		equal(l, "(topology-independent? *R*  #R(2 3))", F);
		equal(l, "(topology-independent? *Q*  #R(2 3))", F);
		equal(l, "(topology-independent? *Z*  #R(2 3))", T);
		equal(l, "(topology-independent? *N*  #R(2 3))", T);
		equal(l, "(topology-independent? *N+* #R(2 3))", T);

		equal(l, "(topology-independent? *O*  #R(2 3])", F);
		equal(l, "(topology-independent? *H*  #R(2 3])", F);
		equal(l, "(topology-independent? *C*  #R(2 3])", F);
		equal(l, "(topology-independent? *R*  #R(2 3])", F);
		equal(l, "(topology-independent? *Q*  #R(2 3])", F);
		equal(l, "(topology-independent? *Z*  #R(2 3])", F);
		equal(l, "(topology-independent? *N*  #R(2 3])", F);
		equal(l, "(topology-independent? *N+* #R(2 3])", F);

		equal(l, "(topology-independent? *O*  #R[2 3))", F);
		equal(l, "(topology-independent? *H*  #R[2 3))", F);
		equal(l, "(topology-independent? *C*  #R[2 3))", F);
		equal(l, "(topology-independent? *R*  #R[2 3))", F);
		equal(l, "(topology-independent? *Q*  #R[2 3))", F);
		equal(l, "(topology-independent? *Z*  #R[2 3))", F);
		equal(l, "(topology-independent? *N*  #R[2 3))", F);
		equal(l, "(topology-independent? *N+* #R[2 3))", F);

		equal(l, "(topology-independent? *O*  #R(2 2.993])", F);
		equal(l, "(topology-independent? *H*  #R(2 2.993])", F);
		equal(l, "(topology-independent? *C*  #R(2 2.993])", F);
		equal(l, "(topology-independent? *R*  #R(2 2.993])", F);
		equal(l, "(topology-independent? *Q*  #R(2 2.993])", F);
		equal(l, "(topology-independent? *Z*  #R(2 2.993])", T);
		equal(l, "(topology-independent? *N*  #R(2 2.993])", T);
		equal(l, "(topology-independent? *N+* #R(2 2.993])", T);

		equal(l, "(topology-independent? *O*  #R(2 4))", F);
		equal(l, "(topology-independent? *H*  #R(2 4))", F);
		equal(l, "(topology-independent? *C*  #R(2 4))", F);
		equal(l, "(topology-independent? *R*  #R(2 4))", F);
		equal(l, "(topology-independent? *Q*  #R(2 4))", F);
		equal(l, "(topology-independent? *Z*  #R(2 4))", F);
		equal(l, "(topology-independent? *N*  #R(2 4))", F);
		equal(l, "(topology-independent? *N+* #R(2 4))", F);

		equal(l, "(topology-independent? #R(2 4) *O*)",  F);
		equal(l, "(topology-independent? #R(2 4) *H*)",  F);
		equal(l, "(topology-independent? #R(2 4) *C*)",  F);
		equal(l, "(topology-independent? #R(2 4) *R*)",  F);
		equal(l, "(topology-independent? #R(2 4) *Q*)",  F);
		equal(l, "(topology-independent? #R(2 4) *Z*)",  F);
		equal(l, "(topology-independent? #R(2 4) *N*)",  F);
		equal(l, "(topology-independent? #R(2 4) *N+*)", F);
	}

}
