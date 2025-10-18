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
package net.morilib.lisp.unicode;

import net.morilib.lisp.subr.IsCharType;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/03/24
 */
public class IsCharTitleCase extends IsCharType {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.IsCharType#check(char)
	 */
	@Override
	protected boolean check(char ch) {
		return Character.isTitleCase(ch);
	}

}
