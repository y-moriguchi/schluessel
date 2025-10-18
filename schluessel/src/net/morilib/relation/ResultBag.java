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
public interface ResultBag<T> {

	/**
	 * 
	 * @param row
	 * @return
	 * @throws RelationException 
	 */
	public DBRow<T> referRow(int row) throws RelationException;

	/**
	 * 
	 * @return
	 */
	public boolean isDeletable();

	/**
	 * 
	 * @param row
	 * @throws RelationException 
	 */
	public void deleteRow(int row) throws RelationException;

	/**
	 * 
	 * @return
	 */
	public boolean isInsertable();

	/**
	 * 
	 * @param objects
	 * @throws RelationException 
	 */
	public void insertRow(T... objects) throws RelationException;

	/**
	 * 
	 * @return
	 * @throws RelationException 
	 */
	public DBRow<T> getRow() throws RelationException;

	/**
	 * 
	 * @return
	 */
	public ResultBag<T> rest() throws RelationException;

}
