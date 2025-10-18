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
package net.morilib.lisp.graph;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Queue;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.LispUtils;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/01
 */
public final class LispGraphs {

	//
	private LispGraphs() {}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static boolean hasCycledPath(ILispVertex v,
			Datum label) {
		Queue<ILispVertex> x;
		Set<ILispVertex> s;

		x = new LinkedList<ILispVertex>();
		s = new HashSet<ILispVertex>();
		x.offer(v);
		while(!x.isEmpty()) {
			for(ILispEdge e : x.poll().getEdges()) {
				ILispVertex t = e.getTerminal();

				if(label != null &&
						!LispUtils.equals(label, e.getLabel())) {
					// do nothing
				} else if(t.equals(v)) {
					return true;
				} else if(s.contains(t)) {
					return false;
				} else {
					s.add(t);
					x.offer(t);
				}
			}
		}
		return false;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static boolean hasCycledPath(ILispVertex v) {
		return hasCycledPath(v, null);
	}

	
	public static List<? extends ILispVertex> getCycledPath(
			ILispVertex v, Datum label) {
		Queue<List<ILispVertex>> x;
		Set<ILispVertex> s;
		List<ILispVertex> l;

		x = new LinkedList<List<ILispVertex>>();
		s = new HashSet<ILispVertex>();
		l = new ArrayList<ILispVertex>();
		l.add(v);
		x.offer(l);
		while(!x.isEmpty()) {
			List<ILispVertex> m = x.poll();

			for(ILispEdge e : m.get(m.size() - 1).getEdges()) {
				ILispVertex t = e.getTerminal();

				if(label != null &&
						!LispUtils.equals(label, e.getLabel())) {
					// do nothing
				} else if(t.equals(v)) {
					m.add(t);
					return m;
				} else if(s.contains(t)) {
					return null;
				} else {
					l = new ArrayList<ILispVertex>(m);
					l.add(t);
					s.add(t);
					x.offer(l);
				}
			}
		}
		return null;
	}

	/**
	 * 
	 * @param v
	 * @return
	 */
	public static List<? extends ILispVertex> getCycledPath(
			ILispVertex v) {
		return getCycledPath(v, null);
	}

}
