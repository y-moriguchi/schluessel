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
package net.morilib.lisp.math.random;

import net.morilib.lisp.Datum2;
import net.morilib.math.random.RandomState;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/12/24
 */
public class LispRandomSourceState extends Datum2 {

	//
	/*package*/ RandomState state;

	/**
	 * 
	 * @param state
	 */
	public LispRandomSourceState(RandomState state) {
		this.state = state;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<random-source-state>");
	}

}
