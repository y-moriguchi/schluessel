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
package net.morilib.util.set.quotient;

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import net.morilib.lang.Equivalence;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/24
 */
public final class Classifier {
	
	//
	private Classifier() { }
	
	/**
	 * 
	 * @param set
	 * @return
	 */
	public static<E, Q> Map<Q, Set<E>> classify(
			Set<E> set, Equivalence<E, Q> eqv) {
		Map<Q, Set<E>> res = new HashMap<Q, Set<E>>();
		
		for(E e : set) {
			Q q = eqv.classify(e);
			Set<E> s0;
			
			s0 = res.get(q);
			if(s0 == null) {
				s0 = new HashSet<E>();
				res.put(q, s0);
			}
			s0.add(e);
		}
		return Collections.unmodifiableMap(res);
	}
	
	/**
	 * 
	 * @param set
	 * @return
	 */
	public static<E, Q, V> Map<Q, Set<V>> classify(
			Map<E, V> map, Equivalence<E, Q> eqv) {
		Map<Q, Set<V>> res = new HashMap<Q, Set<V>>();
		
		for(Map.Entry<E, V> e : map.entrySet()) {
			Q q = eqv.classify(e.getKey());
			Set<V> s0;
			
			s0 = res.get(q);
			if(s0 == null) {
				s0 = new HashSet<V>();
				res.put(q, s0);
			}
			s0.add(e.getValue());
		}
		return Collections.unmodifiableMap(res);
	}
	
}
