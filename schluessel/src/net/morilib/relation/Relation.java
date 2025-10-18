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
package net.morilib.relation;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/02/06
 */
public interface Relation<T> {

	/**
	 * 
	 * @return
	 */
	public RelationCursor<T> getCursor();

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> union(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> intersect(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> subtract(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> directProduct(
			Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> equijoin(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> naturalJoin(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> semijoin(Relation<T> r);

	/**
	 * 
	 * @param r
	 * @return
	 */
	public Relation<T> divide(Relation<T> r);

	/**
	 * 
	 * @param cond
	 * @return
	 */
	public Relation<T> restrict(RelationCondition<T> cond);

	/**
	 * 
	 * @param columns
	 * @return
	 */
	public Relation<T> project(Object... columns);

	/**
	 * 
	 * @return
	 */
	public int size() throws RelationException;

}
