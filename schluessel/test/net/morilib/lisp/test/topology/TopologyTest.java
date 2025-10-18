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

import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.test.TCSubr;
import net.morilib.lisp.topology.LispNumberClassTopology;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/23
 */
public class TopologyTest extends TCSubr {

	public void testTopologyIntersection() {
		Scheme l = Scheme.newInstance();

		equal(l,"(topology-intersection *H* *C*)", LispNumberClassTopology.C);
		equal(l,"(topology-intersection #f *C*)", LispBoolean.FALSE);
		equal(l,"(topology-intersection #t *C*)", LispNumberClassTopology.C);
	}

	public void testTopologyUnion() {
		Scheme l = Scheme.newInstance();

		equal(l,"(topology-union *H* *C*)", LispNumberClassTopology.H);
		equal(l,"(topology-union #t *C*)", LispBoolean.TRUE);
		equal(l,"(topology-union #f *C*)", LispNumberClassTopology.C);
		equal(l,"(neighborhood-of? (topology-union *all-chars* *R*) #\\a)", T);
		equal(l,"(neighborhood-of? (topology-union *all-chars* *R*) 1)", T);
		equal(l,"(neighborhood-of? (topology-union *all-chars* *R*) 1+i)", F);
	}

}
