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
package net.morilib.lisp.charset;

/*package*/ interface CharSetPropertyHandler {

	/**
	 * 
	 * @param name
	 */
	public void charsetName(String name);

	/**
	 * 
	 * @param ch
	 */
	public void singleChar(int ch);

	/**
	 * 
	 * @param cb
	 * @param ce
	 */
	public void rangedChar(int cb, int ce);

}