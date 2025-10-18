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
package net.morilib.util.set2;

import java.util.Set;

import net.morilib.lang.algebra.BooleanElement;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
public interface
BooleanAlgebricSet<E, S extends BooleanAlgebricSet<E, S>>
extends Set<E>, BooleanElement<S> {
	
	
	public UniversalSet<E, S> universalSet();
	
}
