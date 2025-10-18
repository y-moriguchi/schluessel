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
 * @author MORIGUCHI, Yuichiro 2011/11/11
 */
public class RealNumberSetTest extends TCSubr {

	public void testIsNeighborhoodOf() {
		Scheme l = Scheme.newInstance();

		equal(l, "(neighborhood-of? #R[0 1] 1/2)", T);
		equal(l, "(neighborhood-of? #R[0 1] 0.5)", T);
		equal(l, "(neighborhood-of? #R[0 1] 1)", T);
		equal(l, "(neighborhood-of? #R[0 1) 1)", F);
	}

	public void testIsTopologyContained() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-contained? #R[0 1/2] #R[0 1])", T);
		equal(l, "(topology-contained? #R(0 1/2] #R[0 1])", T);
		equal(l, "(topology-contained? #R(0 1) #R[0 1])", T);
		equal(l, "(topology-contained? #R(0 1] #R[0 1])", T);
		equal(l, "(topology-contained? #R[0 1) #R[0 1])", T);
		equal(l, "(topology-contained? #R[0 1] #R[0 1])", T);
		equal(l, "(topology-contained? #R[0 1] #R(0 1])", F);
		equal(l, "(topology-contained? #R[0 1] #R[0 1))", F);
		equal(l, "(topology-contained? #R[0 1] #R(0 1))", F);
		equal(l, "(topology-contained? #R[0 1] #R[0 +inf.0))", T);
		equal(l, "(topology-contained? #R[0 1] #R(0 +inf.0))", F);
		equal(l, "(topology-contained? #R() #R(-inf.0 +inf.0))", T);
	}

	public void testIsIndependent() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-independent? #R[0 1] #R[-1 0])", F);
		equal(l, "(topology-independent? #R[0 1] #R[-1 0))", T);
		equal(l, "(topology-independent? #R[-1 0] #R[0 +inf.0))", F);
		equal(l, "(topology-independent? #R[-1 0] #R(0 +inf.0))", T);
		equal(l, "(topology-independent? #R(-inf.0 0] #R[0 1))", F);
		equal(l, "(topology-independent? #R(-inf.0 0] #R(0 1))", T);
		equal(l, "(topology-independent? #R() #R(-inf.0 +inf.0))", T);
	}

	public void testIsEqv() {
		Scheme l = Scheme.newInstance();

		equal(l, "(eqv? #R[0 1] #R[0 1])", T);
		equal(l, "(eqv? #R[0 1] #R(0 1])", F);
		equal(l, "(eqv? #R[0 1] #R[-1 1])", F);
		equal(l, "(eqv? #R[0 +inf.0) #R[0 +inf.0))", T);
		equal(l, "(eqv? #R(-inf.0 +inf.0) #R(-inf.0 +inf.0))", T);		
	}

	public void testUnionTopology() {
		Scheme l = Scheme.newInstance();

		equal(l, "(eqv? (topology-union #R[0 1] #R[-1 0]) #R[-1 1])", T);
		equal(l, "(eqv? (topology-union #R[0 1) #R(-1 0]) #R(-1 1))", T);
		equal(l, "(eqv? (topology-union #R[0 1] #R[-1 0)) #R[-1 1])", T);
		equal(l, "(eqv? (topology-union #R[0 1] #R[-1 0] #R[0 2]) #R[-1 2])", T);
	}

	public void testIntersectionTopology() {
		Scheme l = Scheme.newInstance();

		equal(l, "(eqv? (topology-intersection #R[0 1] #R[-1 1/2]) #R[0 1/2])", T);
		equal(l, "(eqv? (topology-intersection #R(0 1] #R[-1 1/2)) #R(0 1/2))", T);
		equal(l, "(eqv? (topology-intersection #R[0 1] #R[-1 0]) #R[0 0])", T);
		equal(l, "(eqv? (topology-intersection #R[0 1) #R(-1 0]) #R[0 0])", T);
		equal(l, "(eqv? (topology-intersection #R[0 1] #R[-1 0)) #R())", T);
		equal(l, "(eqv? (topology-intersection #R[0 1] #R[-1 0] #R[0 2]) #R[0 0])", T);
		equal(l, "(eqv? (topology-intersection #R[-1/2 1] #R[-1 0] #R[-2 2]) #R[-1/2 0])", T);
	}

	public void testTopologyInterior() {
		Scheme l = Scheme.newInstance();

		equal(l, "(eqv? (topology-interior #R[0 1]) #R(0 1))", T);
		equal(l, "(eqv? (topology-interior #R(0 1)) #R(0 1))", T);
		equal(l, "(eqv? (topology-interior (topology-interior #R[0 1])) #R(0 1))", T);
		equal(l, "(eqv? (topology-interior #R[0 +inf.0)) #R(0 +inf.0))", T);
		equal(l, "(eqv? (topology-interior #R(-inf.0 0]) #R(-inf.0 0))", T);
		equal(l, "(eqv? (topology-interior #R(-inf.0 +inf.0)) #R(-inf.0 +inf.0))", T);
	}

	public void testTopologyClosure() {
		Scheme l = Scheme.newInstance();

		equal(l, "(eqv? (topology-closure #R[0 1]) #R[0 1])", T);
		equal(l, "(eqv? (topology-closure #R(0 1)) #R[0 1])", T);
		equal(l, "(eqv? (topology-closure (topology-interior #R(0 1))) #R[0 1])", T);
		equal(l, "(eqv? (topology-closure #R(0 +inf.0)) #R[0 +inf.0))", T);
		equal(l, "(eqv? (topology-closure #R(-inf.0 0)) #R(-inf.0 0])", T);
		equal(l, "(eqv? (topology-closure #R(-inf.0 +inf.0)) #R(-inf.0 +inf.0))", T);
	}

	public void testIsOpen() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-open? #R(0 1))", T);
		equal(l, "(topology-open? #R(0 1])", F);
		equal(l, "(topology-open? #R[0 1))", F);
		equal(l, "(topology-open? #R[0 1])", F);
		equal(l, "(topology-open? #R(0 +inf.0))", T);
		equal(l, "(topology-open? #R[0 +inf.0))", F);
		equal(l, "(topology-open? #R(-inf.0 0))", T);
		equal(l, "(topology-open? #R(-inf.0 0])", F);
		equal(l, "(topology-open? #R(-inf.0 +inf.0))", T);
	}

	public void testIsClosed() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-closed? #R(0 1))", F);
		equal(l, "(topology-closed? #R(0 1])", F);
		equal(l, "(topology-closed? #R[0 1))", F);
		equal(l, "(topology-closed? #R[0 1])", T);
		equal(l, "(topology-closed? #R(0 +inf.0))", F);
		equal(l, "(topology-closed? #R[0 +inf.0))", T);
		equal(l, "(topology-closed? #R(-inf.0 0))", F);
		equal(l, "(topology-closed? #R(-inf.0 0])", T);
		equal(l, "(topology-closed? #R(-inf.0 +inf.0))", T);
	}

	public void testTopologyCardinality() {
		Scheme l = Scheme.newInstance();

		equal(l, "(topology-cardinality #R[0 1])", LispCardinality.C);
		equal(l, "(topology-cardinality #R[0 0])", LispCardinality.finiteValueOf(1));
		equal(l, "(topology-cardinality #R())", LispCardinality.finiteValueOf(0));
	}

	public void testSetMaximum() {
		Scheme l = Scheme.newInstance();

		equal(l, "(set-maximum #R[0 1])", newZ(1));
		equal(l, "(set-maximum #R(0 1])", newZ(1));
		equal(l, "(set-maximum #R[0 1))", F);
		equal(l, "(set-maximum #R(0 1))", F);
		equal(l, "(set-maximum #R[0 +inf.0))", F);
		equal(l, "(set-maximum #R(-inf.0 0])", newZ(0));
		equal(l, "(set-maximum #R(-inf.0 +inf.0))", F);
		equal(l, "(set-maximum (topology-union #R[0 1] #R[2 3]))", newZ(3));
	}

	public void testSetMinimum() {
		Scheme l = Scheme.newInstance();

		equal(l, "(set-minimum #R[0 1])", newZ(0));
		equal(l, "(set-minimum #R(0 1])", F);
		equal(l, "(set-minimum #R[0 1))", newZ(0));
		equal(l, "(set-minimum #R(0 1))", F);
		equal(l, "(set-minimum #R[0 +inf.0))", newZ(0));
		equal(l, "(set-minimum #R(-inf.0 0])", F);
		equal(l, "(set-minimum #R(-inf.0 +inf.0))", F);
		equal(l, "(set-minimum (topology-union #R[0 1] #R[2 3]))", newZ(0));
	}

	public void testSetSupremum() {
		Scheme l = Scheme.newInstance();

		equal(l, "(set-supremum #R[0 1])", newZ(1));
		equal(l, "(set-supremum #R(0 1])", newZ(1));
		equal(l, "(set-supremum #R[0 1))", newZ(1));
		equal(l, "(set-supremum #R(0 1))", newZ(1));
		equal(l, "(set-supremum #R[0 +inf.0))", newR(Double.POSITIVE_INFINITY));
		equal(l, "(set-supremum #R(-inf.0 0])", newZ(0));
		equal(l, "(set-supremum #R(-inf.0 +inf.0))", newR(Double.POSITIVE_INFINITY));
		equal(l, "(set-supremum (topology-union #R[0 1] #R[2 3]))", newZ(3));
	}

	public void testSetInfimum() {
		Scheme l = Scheme.newInstance();

		equal(l, "(set-infimum #R[0 1])", newZ(0));
		equal(l, "(set-infimum #R(0 1])", newZ(0));
		equal(l, "(set-infimum #R[0 1))", newZ(0));
		equal(l, "(set-infimum #R(0 1))", newZ(0));
		equal(l, "(set-infimum #R[0 +inf.0))", newZ(0));
		equal(l, "(set-infimum #R(-inf.0 0])", newR(Double.NEGATIVE_INFINITY));
		equal(l, "(set-infimum #R(-inf.0 +inf.0))", newR(Double.NEGATIVE_INFINITY));
		equal(l, "(set-infimum (topology-union #R[0 1] #R[2 3]))", newZ(0));
	}

}
