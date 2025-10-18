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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.LispUtils;
import net.morilib.util.mapset.HashOneToOneSet;
import net.morilib.util.mapset.OneToOneSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/01
 */
public class LispDirectedGraph extends Datum2 implements ILispGraph {

	//
	/*package*/ static class Vert extends Datum2
	implements ILispVertex {

		//
		private LispDirectedGraph graph;

		//
		/*package*/ Vert(LispDirectedGraph g) {
			this.graph = g;
		}

		//
		public Collection<? extends ILispVertex> getTerminals() {
			return Collections.unmodifiableCollection(
					graph.vertices.get(this).values());
		}

		//
		public Collection<? extends ILispVertex> getTerminals(
				Datum label) {
			Map<Edge, Vert> es = graph.vertices.get(this);
			List<Vert> vs = new ArrayList<Vert>();

			for(Map.Entry<Edge, Vert> e : es.entrySet()) {
				if(label == null || LispUtils.equals(
						e.getKey().getLabel(), label)) {
					vs.add(e.getValue());
				}
			}
			return Collections.unmodifiableList(vs);
		}

		//
		public Collection<? extends ILispEdge> getEdges() {
			Map<Edge, Vert> v = graph.vertices.get(this);

			if(v != null) {
				return Collections.unmodifiableSet(v.keySet());
			} else {
				return Collections.emptySet();
			}
		}

		//
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<vertex ")
			.append(LispUtils.print(graph.vertexNames.getValue(this)))
			.append(">");
		}

	}

	//
	/*package*/ static class Edge extends Datum2
	implements ILispEdge {

		//
		private Vert  init, term;
		private Datum label;

		//
		/*package*/ Edge(Vert init, Vert term, Datum label) {
			this.init  = init;
			this.term  = term;
			this.label = label;
		}

		//
		public Datum getLabel() {
			return label;
		}

		//
		public ILispVertex getInitial() {
			return init;
		}

		//
		public ILispVertex getTerminal() {
			return term;
		}

		//
		@Override
		public void toDisplayString(StringBuilder buf) {
			buf.append("#<edge ")
			.append(LispUtils.print(init))
			.append(" -- ")
			.append(LispUtils.print(label))
			.append(" -> ")
			.append(LispUtils.print(term))
			.append(">");
		}

	}

	//
	/*package*/ Map<Vert, Map<Edge, Vert>> vertices;
	/*package*/ OneToOneSet<Vert, Datum> vertexNames;
	/*package*/ Collection<Datum> vertexData;

	/**
	 * 
	 */
	public LispDirectedGraph() {
		vertices    = new HashMap<Vert, Map<Edge, Vert>>();
		vertexNames = new HashOneToOneSet<Vert, Datum>();
		vertexData  = new ArrayList<Datum>();
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.graph.ILispDirectedGraph#getVertices()
	 */
	public Set<? extends ILispVertex> getVertices() {
		return Collections.unmodifiableSet(vertexNames.keySet());
	}

	/**
	 * 
	 * @param init
	 * @param term
	 * @param label
	 */
	public void addEdge(Datum init, Datum term, Datum label) {
		Vert in, te;
		Map<Edge, Vert> mp;
		Edge ed;

		if((in = vertexNames.getKey(init)) == null) {
			in = new Vert(this);
			vertexNames.put(in, init);
			vertexData.add(init);
		}

		if((te = vertexNames.getKey(term)) == null) {
			te = new Vert(this);
			vertexNames.put(te, term);
			vertexData.add(term);
		}

		ed = new Edge(in, te, label);
		if((mp = vertices.get(in)) == null) {
			mp = new LinkedHashMap<Edge, Vert>();
			vertices.put(in, mp);
		}
		mp.put(ed, te);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.graph.ILispDirectedGraph#getVertexNames()
	 */
	public Set<? extends Datum> getVertexNames() {
		return Collections.unmodifiableSet(
				new LinkedHashSet<Datum>(vertexData));
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.graph.ILispDirectedGraph#getVertex(net.morilib.lisp.Datum)
	 */
	public ILispVertex getVertex(Datum d) {
		return vertexNames.getKey(d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.graph.ILispDirectedGraph#getDatum(net.morilib.lisp.graph.ILispDirectedVertex)
	 */
	public Datum getDatum(ILispVertex v) {
		return vertexNames.getValue(v);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.graph.ILispDirectedGraph#addVertex(net.morilib.lisp.Datum)
	 */
	public void addVertex(Datum d) {
		vertexData.add(d);
		vertexNames.put(new Vert(this), d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<directed graph>");
	}

}
