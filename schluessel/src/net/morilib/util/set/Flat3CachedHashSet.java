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
package net.morilib.util.set;

import java.util.HashSet;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/10/05
 */
public class Flat3CachedHashSet<E> extends Flat3CachedSet<E>
implements java.io.Serializable {

	//
	private static final long serialVersionUID = -3939694437566526417L;

	/* (non-Javadoc)
	 * @see net.morilib.util.set.CachedSet#createSet()
	 */
	@Override
	protected Set<E> createSet() {
		return new HashSet<E>();
	}

}
