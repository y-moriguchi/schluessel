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

import java.util.Set;

import net.morilib.lisp.Datum;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/10/02
 */
public interface ILispGraph {


	/**
	 * 
	 * @return
	 */
	public Set<? extends ILispVertex> getVertices();

	/**
	 * 
	 * @param d
	 * @return
	 */
	public ILispVertex getVertex(Datum d);

	/**
	 * 
	 * @param v
	 * @return
	 */
	public Datum getDatum(ILispVertex v);
/**
	 * 
	 * @return
	 */
	public Set<? extends Datum> getVertexNames();

	/**
	 * 
	 * @param d
	 */
	public void addVertex(Datum d);

	/**
	 * 
	 * @param init
	 * @param term
	 * @param label
	 */
	public void addEdge(Datum init, Datum term, Datum label);

}
