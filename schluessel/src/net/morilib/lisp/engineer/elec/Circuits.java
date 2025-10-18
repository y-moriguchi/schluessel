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
package net.morilib.lisp.engineer.elec;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispNumber;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.graph.ILispEdge;
import net.morilib.lisp.graph.ILispGraph;
import net.morilib.lisp.graph.ILispVertex;
import net.morilib.lisp.math.matrix.ILispMatrix;
import net.morilib.lisp.math.matrix.LispMatrix;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/02
 */
public class Circuits {

	//
	private static final Symbol EQ = Symbol.getSymbol("=");

	/**
	 * 
	 * @param g
	 * @param mesg
	 * @return
	 */
	public static ILispMatrix toCircuitMatrix(ILispGraph g,
			LispMessage mesg) {
		OneToOneSet<Integer, Datum> mp;
		int ii = 0;
		LispMatrix m;
		ILispVertex d;
	
		mp = new HashOneToOneSet<Integer, Datum>();
		for(Datum x : g.getVertexNames()) {
			if(x.isTrue()) {
				mp.put(ii++, x);
			}
		}
	
		m = new LispMatrix(mp.size(), mp.size());
		for(int i = 0; i < mp.size(); i++) {
			d = g.getVertex(mp.getValue(i));
			for(ILispEdge e : d.getEdges()) {
				if(EQ.equals(e.getLabel())) {
					// gyrator
					Integer j = mp.getKey(g.getDatum(e.getTerminal()));

					if(j == null) {
						throw mesg.getError(
								"err.circuit.graph.invalid");
					}
					m.set(i, j, LispInteger.valueOf(-1));
					m.set(j, i, LispInteger.valueOf(1));
				} else if(!(e.getLabel() instanceof LispNumber)) {
					throw mesg.getError("err.require.number",
							e.getLabel());
				} else if(g.getDatum(e.getTerminal()).isTrue()) {
					LispNumber n = (LispNumber)e.getLabel();
					int j = mp.getKey(g.getDatum(e.getTerminal()));
	
					m.set(i, i, m.get(i, i).add(n));
					m.set(j, j, m.get(j, j).add(n));
					m.set(i, j, m.get(i, j).sub(n));
					m.set(j, i, m.get(j, i).sub(n));
				} else {
					LispNumber n = (LispNumber)e.getLabel();

					m.set(i, i, m.get(i, i).add(n));
				}
			}
		}

		// from ground
		d = g.getVertex(LispBoolean.FALSE);
		for(ILispEdge e : d.getEdges()) {
			if(EQ.equals(e.getLabel())) {
				throw mesg.getError(
						"err.circuit.graph.invalid");
			} else if(!(e.getLabel() instanceof LispNumber)) {
				throw mesg.getError("err.require.number",
						e.getLabel());
			} else {
				LispNumber n = (LispNumber)e.getLabel();
				int j = mp.getKey(g.getDatum(e.getTerminal()));

				m.set(j, j, m.get(j, j).add(n));
			}
		}
		return m;
	}

}
