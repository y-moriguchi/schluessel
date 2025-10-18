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
package net.morilib.util.bag;

import java.util.Collection;
import java.util.Set;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/30
 */
public interface Bag<E> extends Collection<E> {
	
	
	public int multiplicity(Object o);
	
	
	public boolean add(E a, int coefficient);
	
	
	public void multiply(int n);
	
	
	public boolean containsAllAsBag(Collection<?> c);
	
	
	public boolean removeElements(Object o);
	
	
	public boolean remove(Object o, int coefficient);
	
	
	public boolean removeAllAsBag(Collection<?> c);
	
	
	public boolean retainAllAsBag(Collection<?> c);
	
	
	public Set<E> unique();
	
	
	public Bag<E> subBagBelow(int multiplicity);
	
	
	public Bag<E> subBagAbove(int multiplicity);
	
	
	public Bag<E> subBagEquals(int multiplicity);
	
}
