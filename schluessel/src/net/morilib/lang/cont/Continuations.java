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
package net.morilib.lang.cont;

import java.io.Serializable;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2010
 */
public final class Continuations {

	private Continuations() { }

	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010
	 */
	public static class KClass {

		//
		private KClass() {}


		@Continuatable
		public Object execute(Object s) {
			return s;
		}

	}

	/**
	 * 
	 */
	public static final KClass K = new KClass();

	/**
	 * 
	 */
	public static final Serializable ADD_INT2 = new Serializable() {

		//
		private static final long serialVersionUID = -247120899698518733L;

		@Continuatable
		Integer execute(Integer x, Integer y) {
			if(x == null) {
				return (y == null) ? null : y;
			} else {
				return (y == null) ? x : x + y;
			}
		}

	};

	/**
	 * 
	 */
	public static final Serializable SUB_INT2 = new Serializable() {

		//
		private static final long serialVersionUID = -5419312055177649355L;

		@Continuatable
		Integer execute(Integer x, Integer y) {
			if(x == null) {
				return (y == null) ? null : -y;
			} else {
				return (y == null) ? x : x - y;
			}
		}

	};

}
