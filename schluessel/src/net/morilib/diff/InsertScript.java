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
package net.morilib.diff;

import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/10
 */
public class InsertScript<T> implements EditScript<T> {

	//
	private int index;
	private T   object;
	private EditScript<T> prev;

	/**
	 * 
	 * @param index
	 * @param object
	 * @param prev
	 */
	public InsertScript(int index, T object, EditScript<T> prev) {
		this.index  = index;
		this.object = object;
		this.prev   = prev;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#getIndex()
	 */
	public int getIndex() {
		return index;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#getInsertObject()
	 */
	public T getObject() {
		return object;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#isDelete()
	 */
	public boolean isDelete() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#getPrevious()
	 */
	public EditScript<T> getPrevious() {
		return prev;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.Patch#patch(java.util.List)
	 */
	public int edit(List<T> list, int inserted) {
		list.add(index + inserted, object);
		return inserted + 1;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "Insert " + object + " after symbol " + index;
	}

}
