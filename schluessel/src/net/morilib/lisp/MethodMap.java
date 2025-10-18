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
package net.morilib.lisp;

import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;
import java.util.TreeSet;

import net.morilib.lisp.sos.LispType;
import net.morilib.lisp.sos.LispTypeList;

/*package*/ class MethodMap {
	
	private static class Selected implements Iterable<LispTypeList> {
		
		private class Elem implements Comparable<Elem> {
			
			private LispTypeList type;
			
			//
			private Elem(LispTypeList type) {
				this.type = type;
			}
			
			public int compareTo(Elem o) {
				for(int i = 0; i < argt.size(); i++) {
					if(i >= o.type.size() && o.type.isRest()) {
						return -1;
					} else if(i >= type.size() && type.isRest()) {
						return 1;
					} else {
						LispType t1 = type.get(i);
						LispType t2 = o.type.get(i);
						
						if(!t1.equals(t2)) {
							List<LispType> cpl = argt.get(i).getCPL();
							int c1 = cpl.indexOf(t1);
							int c2 = cpl.indexOf(t2);
							
							if(c1 < 0 && c2 < 0) {
								return 0;
							} else if(c1 < 0) {
								return -1;
							} else if(c2 < 0) {
								return 1;
							} else {
								return (c1 < c2) ?
										-1 : (c1 > c2) ? 1 : 0;
							}
						}
					}
				}
				return 0;
			}
			
		}
		
		private static class Iter implements Iterator<LispTypeList> {
			
			private Iterator<Elem> iter;
			
			private Iter(Iterator<Elem> elemi) {
				this.iter = elemi;
			}
			
			public boolean hasNext() {
				return iter.hasNext();
			}

			public LispTypeList next() {
				return iter.next().type;
			}

			public void remove() {
				throw new UnsupportedOperationException();
			}
			
		}
		
		private LispTypeList argt;
		private SortedSet<Elem> elems = new TreeSet<Elem>();
		
		private Selected(LispTypeList e) {
			argt = e;
		}
		
		public Iterator<LispTypeList> iterator() {
			return new Iter(elems.iterator());
		}
		
	}
	
	//
	private Map<LispTypeList, Closure> types =
		new HashMap<LispTypeList, Closure>();
	//private Set<LispTypeList> rest = new HashSet<LispTypeList>();
	
	
	public Iterable<LispTypeList> select(LispTypeList arg) {
		Selected seld = new Selected(arg);
		
		outer:
		for(LispTypeList e : types.keySet()) {
			if(!(e.size() == arg.size() ||
					(e.size() < arg.size() && e.isRest()))) {
				continue;
			}
			
			for(int i = 0; i < e.size(); i++) {
				List<LispType> cpl = arg.get(i).getCPL();
				
				if(!cpl.contains(e.get(i))) {
					continue outer;
				}
			}
			seld.elems.add(seld.new Elem(e));
		}
		return seld;
	}
	
	
	public void put(LispTypeList type, Closure clo) {
		types.put(type, clo);
		//if(rest) {
		//	this.rest.add(type);
		//}
	}
	
	
	public boolean remove(LispTypeList type) {
		return types.remove(type) != null;
	}
	
	
	public Closure get(LispTypeList type) {
		return types.get(type);
	}
	
	
	public int size() {
		return types.size();
	}
	
}
