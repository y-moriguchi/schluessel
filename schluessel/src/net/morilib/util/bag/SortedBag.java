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

import java.util.Comparator;
import java.util.SortedSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/30
 */
public interface SortedBag<E> extends Bag<E> {
	
	
	public Comparator<? super E> comparator();
	
	
	public SortedBag<E> headBag(E toElement);
	
	
	public SortedBag<E> tailBag(E fromElement);
	
	
	public SortedBag<E> subBagBelow(int multiplicity);
	
	
	public SortedBag<E> subBagAbove(int multiplicity);
	
	
	public SortedBag<E> subBagEquals(int multiplicity);
	
	
	public E first();
	
	
	public E last();
	
	
	public SortedSet<E> unique();

}
