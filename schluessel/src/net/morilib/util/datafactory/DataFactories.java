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
package net.morilib.util.datafactory;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;
import java.util.IdentityHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeMap;
import java.util.TreeSet;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/08/29
 */
public final class DataFactories {
	
	//
	private DataFactories() {}
	
	
	public static final SetFactory
	HASH_SET = new SetFactory() {

		public<E> Set<E> newInstance() {
			return new HashSet<E>();
		}

		public<E> Set<E> clone(Collection<E> c) {
			return new HashSet<E>(c);
		}
		
	};
	
	
	public static final SortedSetFactory
	TREE_SET = new SortedSetFactory() {

		public<E> SortedSet<E> newInstance() {
			return new TreeSet<E>();
		}

		public<E> SortedSet<E> clone(Collection<E> c) {
			return new TreeSet<E>(c);
		}
		
	};
	
	
	public static final ListFactory
	ARRAY_LIST = new ListFactory() {

		public<E> List<E> newInstance() {
			return new ArrayList<E>();
		}

		public<E> List<E> clone(Collection<E> c) {
			return new ArrayList<E>(c);
		}
		
	};
	
	
	public static final ListFactory
	LINKED_LIST = new ListFactory() {

		public<E> List<E> newInstance() {
			return new LinkedList<E>();
		}

		public<E> List<E> clone(Collection<E> c) {
			return new LinkedList<E>(c);
		}
		
	};
	
	
	public static final MapFactory
	HASH_MAP = new MapFactory() {

		public<K, V> Map<K, V> newInstance() {
			return new HashMap<K, V>();
		}

		public<K, V> Map<K, V> clone(Map<K, V> c) {
			return new HashMap<K, V>(c);
		}
		
	};
	
	
	public static final MapFactory
	IDENTITY_HASH_MAP = new MapFactory() {

		public<K, V> Map<K, V> newInstance() {
			return new IdentityHashMap<K, V>();
		}

		public<K, V> Map<K, V> clone(Map<K, V> c) {
			return new IdentityHashMap<K, V>(c);
		}
		
	};
	
	
	public static final SortedMapFactory
	TREE_MAP = new SortedMapFactory() {

		public<K, V> SortedMap<K, V> newInstance() {
			return new TreeMap<K, V>();
		}

		public<K, V> SortedMap<K, V> clone(Map<K, V> c) {
			return new TreeMap<K, V>(c);
		}
		
	};
	
}
