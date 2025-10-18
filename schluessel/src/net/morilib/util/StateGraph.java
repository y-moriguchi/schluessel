/*
 * Copyright 2009 Yuichiro Moriguchi
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
package net.morilib.util;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * A class of state transition graph.
 * <p>状態遷移グラフを実装したクラスです。
 * 
 * 
 * @author MORIGUCHI, Yuichiro 2010/04/18
 */
public class StateGraph<T, S> {
	
	private Map<T, Map<S, T>> graph = new HashMap<T, Map<S, T>>();
	private Map<T, StateIn> stmap = new HashMap<T, StateIn>();
	
	/**
	 * An interface represents states.
	 * <p>状態を表現するインターフェースです。
	 * 
	 * 
	 * @author MORIGUCHI, Yuichiro 2010/04/18
	 */
	public static interface State<T> {
		
		/**
		 * returns new state by the given input.
		 * <p>与えられた入力に対応する状態を得ます。
		 * 
		 * @return new state
		 */
		public State<T> go(T input);
		
	}
	
	private class StateIn implements State<T> {
		
		private T st;
		
		private StateIn(T st) {
			this.st = st;
		}
		
		public State<T> go(T alphabet) {
			return stmap.get(graph.get(st).get(alphabet));
		}

	}
	
	private static class Immutable<T, S> extends StateGraph<T, S> {
		
		private StateGraph<T, S> wrapee;
		
		private Immutable(StateGraph<T, S> w) {
			wrapee = w;
		}
		
		//
		@Override
		public void addNode(T nst) {
			throw new UnsupportedOperationException();
		}

		//
		@Override
		public void addTrans(T st, S i, T nst) {
			throw new UnsupportedOperationException();
		}

		@Override
		public T get(T st, S inp) {
			return wrapee.get(st, inp);
		}

		@Override
		public Set<T> getAllNodes() {
			return wrapee.getAllNodes();
		}

		@Override
		public Map<S, T> getEdgeMap(T st) {
			return wrapee.getEdgeMap(st);
		}

		@Override
		public State<T> getState(T st) {
			return wrapee.getState(st);
		}

		@Override
		public boolean isState(T st) {
			return wrapee.isState(st);
		}
		
		@Override
		public int stateSize() {
			return wrapee.stateSize();
		}

		@Override
		public void addEdgeMap(T st, Map<S, T> edges) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	/**
	 * returns the unmodifiable view of the given graph.
	 * <p>与えられたグラフの変更不可能なビューを得ます。
	 * 
	 * @return the unmodifiable view
	 */
	public static<T, S> StateGraph<T, S> unmodifiableGraph(
			StateGraph<T, S> w) {
		if(w instanceof Immutable<?, ?>) {
			return w;
		} else {
			return new Immutable<T, S>(w);
		}
	}
	
	//
	private Map<S, T> newNode(T nst) {
		Map<S, T> ne = graph.get(nst);
		
		if(ne == null) {
			ne = new HashMap<S, T>();
			graph.put(nst, ne);
			stmap.put(nst, new StateIn(nst));
		}
		return ne;
	}
	
	/**
	 * add a new node to this graph.
	 * <p>このグラフに新しい節点を追加します。
	 * 
	 * @param nst a new node
	 */
	public void addNode(T nst) {
		newNode(nst);
	}
	
	/**
	 * add a new transition to this graph.
	 * <p>このグラフに新しい遷移を追加します。
	 * 
	 * @param st   a node to transit from
	 * @param i    an input to transit
	 * @param nst  a node to transit to
	 */
	public void addTrans(T st, S i, T nst) {
		Map<S, T> edges;
		
		if(st == null || i == null || nst == null) {
			throw new NullPointerException();
		}
		
		edges = graph.get(st);
		if(edges == null) {
			edges = new HashMap<S, T>();
			edges.put(i, nst);
			graph.put(st, edges);
			stmap.put(st, new StateIn(st));
		} else {
			edges.put(i, nst);
		}
		
		addNode(nst);
	}
	
	/**
	 * gets the state corresponds to the given node.
	 * <p>与えられた節点に対応する状態を得ます。
	 * 
	 * @return state
	 */
	public State<T> getState(T st) {
		State<T> res = stmap.get(st);
		
		if(res == null) {
			throw new NullPointerException();
		}
		return res;
	}
	
	/**
	 * gets all nodes of this graph.
	 * <p>このグラフのすべての節点を得ます。
	 */
	public Set<T> getAllNodes() {
		return Collections.unmodifiableSet(graph.keySet());
	}
	
	/**
	 * gets edges of the given node as Map.
	 * <p>与えられた節点の辺をMapとして得ます。
	 * 
	 * @param st node to get
	 * @return edges
	 */
	public Map<S, T> getEdgeMap(T st) {
		return Collections.unmodifiableMap(graph.get(st));
	}
	
	/**
	 * add an edge to this graph.
	 * <p>このグラフにMapとして表現された辺を追加します。
	 * 
	 * @param st node to be added
	 * @param edges edges to add
	 */
	public void addEdgeMap(T st, Map<S, T> edges) {
		Map<S, T> nd = newNode(st);
		
		for(Map.Entry<S, T> e : edges.entrySet()) {
			nd.put(e.getKey(), e.getValue());
		}
	}
	
	/**
	 * gets the node transited from the given node and input.
	 * <p>与えられた節点と入力により遷移する接点を得ます。
	 * 
	 * @param st  node to transit from
	 * @param inp input to transit
	 */
	public T get(T st, S inp) {
		Map<S, T> edg = graph.get(st);
		T res;
		if(st == null || inp == null) {
			throw new NullPointerException();
		} else if(edg == null) {
			throw new IllegalArgumentException();
		}
		
		if((res = edg.get(inp)) == null) {
			throw new IllegalArgumentException();
		} else {
			return res;
		}
	}
	
	/**
	 * returns true if the given node is in this graph.
	 * <p>与えられた節点がこのグラフに存在するときtrueを得ます。
	 * 
	 * @param st node to be tested
	 */
	public boolean isState(T st) {
		return graph.containsKey(st);
	}
	
	/**
	 * returns the number of nodes.
	 * <p>このグラフがもつ節点数を得ます。
	 */
	public int stateSize() {
		return graph.size();
	}
	
}
