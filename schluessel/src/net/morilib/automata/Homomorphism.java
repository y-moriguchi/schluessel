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
package net.morilib.automata;

import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/02/10
 */
public interface Homomorphism<S, T> {

	/**
	 * 
	 * @param a
	 * @return
	 */
	public List<T> get(S a);

	/**
	 * 
	 * @param t
	 * @return
	 */
	public S invert(List<T> ts);

	/**
	 * 
	 * @param ts
	 * @return
	 */
	public S invert(T... ts);

}
