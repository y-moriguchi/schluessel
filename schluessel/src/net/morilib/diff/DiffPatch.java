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

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/10
 */
public class DiffPatch<T> implements Patch<T> {

	//
	private List<EditScript<T>> scripts;

	//
	DiffPatch(List<EditScript<T>> scr) {
		this.scripts = scr;
	}

	/* (non-Javadoc)
	 * @see net.morilib.diff.Patch#patch(java.util.List)
	 */
	public List<T> patch(List<T> before) {
		List<T> r0 = new LinkedList<T>(before);
		int i = 0;

		for(EditScript<T> e : scripts) {
			i = e.edit(r0, i);
		}
		return new ArrayList<T>(r0);
	}

}
