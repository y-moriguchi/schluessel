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
public class InitialScript<T> implements EditScript<T> {

	/**
	 * 
	 */
	public InitialScript() { }

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#getIndex()
	 */
	public int getIndex() {
		return -1;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.EditScript#getInsertObject()
	 */
	public T getObject() {
		return null;
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
		return null;
	}

	/* (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return "Initial";
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.Patch#patch(java.util.List)
	 */
	public int edit(List<T> list, int inserted) {
		return 0;
	}

}
