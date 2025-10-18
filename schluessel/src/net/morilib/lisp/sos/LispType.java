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
package net.morilib.lisp.sos;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.LinkedList;
import java.util.List;

import net.morilib.util.Tuple2;

public final class LispType {
	
	private static final LinkedHashSet<LispType> _EMPTY =
		new LinkedHashSet<LispType>();
	
	
	public static final LispType TOP = new LispType(false);
	
	
	public static final LispType COLLECTION;
	
	
	public static final LispType SEQUENCE;
	
	
	public static final LispType VECTOR;
	
	
	public static final LispType LIST;
	
	
	public static final LispType STRING;
	
	
	public static final LispType NULL;
	
	
	public static final LispType PAIR;
	
	
	public static final LispType CHAR;
	
	
	public static final LispType NUMBER;
	
	
	public static final LispType COMPLEX;
	
	
	public static final LispType REAL;
	
	
	public static final LispType RATIONAL;
	
	
	public static final LispType INTEGER;
	
	
	public static final LispType BOOLEAN;
	
	
	public static final LispType SYMBOL;
	
	
	public static final LispType OBJECT;
	
	
	public static final LispType CLASS;
	
	
	public static final LispType GENERIC;
	
	
	public static final LispType KEYWORD;
	
	
	public static final LispType REGEXP;
	
	
	//
	private List<LispType> supers;
	private transient List<LispType> cpl;
	
	
	private LispType(boolean x) {
		supers = null;   // top level
		cpl = Collections.singletonList(this);
	}
	
	
	static {
		try {
			COLLECTION = new LispType(LispType.TOP);
			SEQUENCE   = new LispType(LispType.COLLECTION);
			VECTOR     = new LispType(LispType.SEQUENCE);
			LIST       = new LispType(LispType.SEQUENCE);
			STRING     = new LispType(LispType.SEQUENCE);
			NULL       = new LispType(LispType.LIST);
			PAIR       = new LispType(LispType.LIST);
			CHAR       = new LispType(LispType.TOP);
			NUMBER     = new LispType(LispType.TOP);
			COMPLEX    = new LispType(LispType.NUMBER);
			REAL       = new LispType(LispType.COMPLEX);
			RATIONAL   = new LispType(LispType.REAL);
			INTEGER    = new LispType(LispType.RATIONAL);
			BOOLEAN    = new LispType(LispType.TOP);
			SYMBOL     = new LispType(LispType.TOP);
			OBJECT     = new LispType(LispType.TOP);
			CLASS      = new LispType(LispType.OBJECT);
			GENERIC    = new LispType(LispType.OBJECT);
			KEYWORD    = new LispType(LispType.TOP);
			REGEXP     = new LispType(LispType.TOP);
		} catch(LispTypeException e) {
			throw new RuntimeException(e);
		}
	}
	
	//
	/*package*/ LispType(
			List<LispType> directSuper) throws LispTypeException {
		initSupers(directSuper);
		computeCPL();
	}
	
	/*package*/ LispType(
			List<LispType> directSuper,
			LispType def) throws LispTypeException {
		initSupers(directSuper);
		this.supers.add(def);
		computeCPL();
	}
	
	/*package*/ LispType(
			LispType... directSuper) throws LispTypeException {
		if(directSuper == null) {
			throw new NullPointerException();
		} else if(directSuper.length < 1) {
			throw new IllegalArgumentException();
		}
		
		this.supers = Arrays.asList(directSuper);
		computeCPL();
	}
	
	private void initSupers(List<LispType> directSuper) {
		if(directSuper == null) {
			throw new NullPointerException();
		} else if(directSuper.size() < 1) {
			throw new IllegalArgumentException();
		}
		
		this.supers = new ArrayList<LispType>(directSuper);
	}
	
	private LinkedHashSet<LispType> computeS() throws LispTypeException {
		if(supers == null) {
			return _EMPTY;
		} else {
			LinkedList<LispType> q = new LinkedList<LispType>(supers);
			LinkedHashSet<LispType> res = new LinkedHashSet<LispType>();
			LinkedHashSet<LispType> dne = new LinkedHashSet<LispType>();
			
			res.add(this);
			res.addAll(supers);
			while(!q.isEmpty()) {
				LinkedHashSet<LispType> s;
				LispType e = q.remove(0);
				
				if(dne.contains(e)) {
					continue;
				}
				
				if(e.supers != null) {
					s = new LinkedHashSet<LispType>(e.supers);
					//s = e.computeS();
					//if(!Sets.isIndependent(dne, s)) {
					//	throw new LispTypeException(
					//			"circulated:" + dne + ":" + s);
					//}
					res.addAll(s);
					q.addAll(s);
				}
				dne.add(e);
			}
			return res;
		}
	}
	
	private LinkedHashSet<Tuple2<LispType, LispType>> getRSet() {
		LinkedHashSet<Tuple2<LispType, LispType>> res =
			new LinkedHashSet<Tuple2<LispType, LispType>>();
		
		if(supers == null) {
			return null;
		} else {
			res.add(new Tuple2<LispType, LispType>(
					this, supers.get(0)));
			for(int i = 1; i < supers.size(); i++) {
				res.add(new Tuple2<LispType, LispType>(
						supers.get(i - 1), supers.get(i)));
			}
			return res;
		}
	}
	
	private boolean isPrecedent(
			LispType e,
			LinkedHashSet<Tuple2<LispType, LispType>> r) {
		for(Tuple2<LispType, LispType> x : r) {
			if(e.equals(x.getB())) {
				return true;
			}
		}
		return false;
	}
	
	private LispType nextE(
			Collection<LispType> s,
			LinkedHashSet<Tuple2<LispType, LispType>> r,
			List<LispType> l) throws LispTypeException {
		if(l.size() > 0) {
			List<LispType> mb = new ArrayList<LispType>();
			
			for(Iterator<LispType> i = s.iterator(); i.hasNext();) {
				LispType e = i.next();
				
				if(isPrecedent(e, r)) {
					//break;
				} else {
					mb.add(e);
				}
			}
			
			if(mb.isEmpty()) {
				throw new LispTypeException();
			}
			
			//for(int i = l.size() - 1; i >= 0; i--) {
			for(int i = 0; i < l.size(); i++) {
				//int t  = Integer.MAX_VALUE;
				//int ri = -1;
				
				for(int j = 0; j < mb.size(); j++) {
					int x = l.get(i).getSuperIndex(mb.get(j));
					
					if(x >= 0) {
						return mb.get(j);
					}
					//if(x >= 0 && x < t) {
					//	t  = x;
					//	ri = j;
					//}
				}
				
				//if(ri >= 0) {
				//	return mb.get(ri);
				//}
			}
			throw new LispTypeException();
		} else {
			Iterator<LispType> i = s.iterator();
			
			return i.next();
		}
	}
	
	private void removeR(
			LinkedHashSet<Tuple2<LispType, LispType>> r, LispType n) {
		Iterator<Tuple2<LispType, LispType>> i = r.iterator();
		
		while(i.hasNext()) {
			Tuple2<LispType, LispType> e = i.next();
			
			if(n.equals(e.getA())) {
				i.remove();
			}
		}
	}
	
	private void computeCPL() throws LispTypeException {
		LinkedHashSet<LispType> s = computeS();
		LinkedHashSet<Tuple2<LispType, LispType>> r =
			new LinkedHashSet<Tuple2<LispType, LispType>>();
		List<LispType> res = new ArrayList<LispType>();
		
		for(LispType e : s) {
			LinkedHashSet<Tuple2<LispType, LispType>> x = e.getRSet();
			
			if(x != null) {
				r.addAll(x);
			}
		}
		
		while(!s.isEmpty()) {
			LispType n = nextE(s, r, res);
			
			s.remove(n);
			removeR(r, n);
			res.add(n);
		}
		
		//if(!s.isEmpty()) {
		//	throw new LispTypeException("contradicted");
		//}
		cpl = res;
	}
	
	
	public int getSuperIndex(LispType e) {
		return supers.indexOf(e);
	}
	
	
	public List<LispType> getSupers() {
		return Collections.unmodifiableList(supers);
	}
	
	
	public List<LispType> getCPL() {
		return Collections.unmodifiableList(cpl);
	}
	
	
	public boolean contains(LispType t) {
		return cpl.contains(t);
	}
	
}
