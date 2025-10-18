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
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Symbol;

public final class SOS {
	
	
	private static final SOS INSTANCE = new SOS();
	
	
	public final LispClass cTop;
	
	
	public final LispClass cCollection;
	
	
	public final LispClass cSequence;
	
	
	public final LispClass cVector;
	
	
	public final LispClass cList;
	
	
	public final LispClass cString;
	
	
	public final LispClass cNull;
	
	
	public final LispClass cPair;
	
	
	public final LispClass cChar;
	
	
	public final LispClass cNumber;
	
	
	public final LispClass cComplex;
	
	
	public final LispClass cReal;
	
	
	public final LispClass cRational;
	
	
	public final LispClass cInteger;
	
	
	public final LispClass cBoolean;
	
	
	public final LispClass cSymbol;
	
	
	public final LispClass cObject;
	
	
	public final LispClass cClass;
	
	
	public final LispClass cGeneric;
	
	
	public final LispClass cKeyword;
	
	
	public final LispClass cRegexp;
	
	
	private Map<LispType, Set<LispType>> subtypes =
		new HashMap<LispType, Set<LispType>>();
	private Map<LispType, Set<LispType>> redefed =
		new HashMap<LispType, Set<LispType>>();
	private Map<LispType, LispClass> klasses =
		new HashMap<LispType, LispClass>();
	
	private LispClass defcls(LispType lt) {
		LispClass res;
		
		res = new LispClass(this, lt);
		klasses.put(lt, res);
		return res;
	}
	
	{
		cTop        = defcls(LispType.TOP);
		cCollection = defcls(LispType.COLLECTION);
		cSequence   = defcls(LispType.SEQUENCE);
		cVector     = defcls(LispType.VECTOR);
		cList       = defcls(LispType.LIST);
		cString     = defcls(LispType.STRING);
		cNull       = defcls(LispType.NULL);
		cPair       = defcls(LispType.PAIR);
		cChar       = defcls(LispType.CHAR);
		cNumber     = defcls(LispType.NUMBER);
		cComplex    = defcls(LispType.COMPLEX);
		cReal       = defcls(LispType.REAL);
		cRational   = defcls(LispType.RATIONAL);
		cInteger    = defcls(LispType.INTEGER);
		cBoolean    = defcls(LispType.BOOLEAN);
		cSymbol     = defcls(LispType.SYMBOL);
		cObject     = defcls(LispType.OBJECT);
		cClass      = defcls(LispType.CLASS);
		cGeneric    = defcls(LispType.GENERIC);
		cKeyword    = defcls(LispType.KEYWORD);
		cRegexp     = defcls(LispType.REGEXP);
	}
	
	
	public static SOS getInstance() {
		return INSTANCE;
	}
	
	
	public LispClass getLispClass(LispType typ) {
		return klasses.get(typ);
	}
	
	
	private List<LispType> gtypes(
			List<LispClass> sup) throws LispTypeException {
		List<LispType> res = new ArrayList<LispType>();
		
		for(LispClass e : sup) {
			LispType t;
			
			//e.refreshClass();
			//e.reinstantiateAll();
			t = e.getObjectType();
			if(!t.contains(LispType.OBJECT)) {
				throw new LispTypeException();
			}
			res.add(e.getObjectType());
		}
		
		if(!res.contains(LispType.OBJECT)) {
			res.add(LispType.OBJECT);
		}
		return res;
	}
	
	private List<LispClass> htypes(
			List<LispType> sup, LispType ot, LispType nt
			) throws LispTypeException {
		List<LispClass> res = new ArrayList<LispClass>();
		
		for(LispType e : sup) {
			res.add(getLispClass(e.equals(ot) ? nt : e));
		}
		return res;
	}
	
	private LispObject gmeta(LispType t) throws LispTypeException {
		List<LispClass>  lm = new ArrayList<LispClass>();
		List<LispObject> om = new ArrayList<LispObject>();
		LispClass mc;
		LispObject res;
		
		for(LispType t0 : t.getCPL()) {
			if(!t.equals(t0)) {
				LispObject ot = getLispClass(t0).getMetaClass();
				LispClass  mt = ot.getLispClass();
				
				if(mt != cClass) {
					om.add(ot);
					if(!lm.contains(mt)) {
						lm.add(mt);
					}
				}
			}
		}
		
		if(lm.size() == 0) {
			mc  = cClass;
			res = mc.instantiate();
		} else if(lm.size() == 1) {
			mc  = lm.get(0);
			res = new LispObject(om.get(0));
		} else {
			Collection<Symbol> e0 = Collections.emptySet();
			
			//lm.add(cClass);
			mc = defineClass(lm, e0, e0, e0, cClass.instantiate());
			res = mc.instantiate();
			for(LispObject o1 : om) {
				res.copyFrom(o1);
			}
		}
		return res;
	}
	
	
	public LispClass defineClass(
			List<LispClass> sup,
			Collection<Symbol> ins,
			Collection<Symbol> cls,
			Collection<Symbol> ecl,
			LispObject meta) throws LispTypeException {
		List<LispType> tl = gtypes(sup);
		LispType t = new LispType(tl);
		LispObject mt2 = (meta == null) ? gmeta(t) : meta;
		LispClass res;
		
		if(!mt2.getType().getCPL().contains(LispType.CLASS)) {
			throw new LispTypeException("err.require.metaclass");
		}
		
		res = new LispClass(null, this, t, ins, cls, ecl, mt2);
		for(LispType e : tl) {
			Set<LispType> s = subtypes.get(e);
			
			if(s == null) {
				s = new HashSet<LispType>();
				subtypes.put(e, s);
			}
			s.add(res.getObjectType());
		}
		klasses.put(t, res);
		return res;
	}
	
	
	private void redefineSubtypes(
			LispClass okls,
			LispClass nkls,
			Map<Symbol, Datum> vals,
			Set<LispType> done) throws LispTypeException {
		Set<LispType> ss = subtypes.get(okls.getObjectType());
		Set<LispType> rd = redefed.get(okls.getObjectType());
		
		if(ss == null) {
			return;
		}
		
		for(LispType t : ss) {
			if((rd == null || !rd.contains(t)) && !done.contains(t)) {
				LispClass c0 = getLispClass(t);
				LispType ot = okls.getObjectType();
				LispType nt = nkls.getObjectType();
				
				redefineClass1(
						c0,
						htypes(t.getSupers(), ot, nt),
						c0.getInstanceSlots(),
						c0.getClassSlots(),
						c0.getEachClassSlots(),
						vals,
						c0.getMetaClass(),
						done);
				done.add(t);
			}
		}
	}
	
	private LispClass redefineClass1(
			LispClass oldkls,
			List<LispClass> sup,
			Collection<Symbol> ins,
			Collection<Symbol> cls,
			Collection<Symbol> ecl,
			Map<Symbol, Datum> vals,
			LispObject meta,
			Set<LispType> done) throws LispTypeException {
		//
		oldkls.reinstantiateAll();
		//oldkls.refreshClass();
		
		//
		List<LispType> tl = gtypes(sup);
		LispType t = new LispType(tl);
		LispObject mt2 = (meta == null) ? gmeta(t) : meta;
		LispClass res;
		
		if(!mt2.getType().getCPL().contains(LispType.CLASS)) {
			throw new LispTypeException("err.require.metaclass");
		}
		
		res = oldkls.redefine(t, ins, cls, ecl, vals, mt2);
		
		for(LispType e : tl) {
			Set<LispType> s = subtypes.get(e);
			
			if(s == null) {
				s = new HashSet<LispType>();
				subtypes.put(e, s);
			}
			s.add(res.getObjectType());
		}
		
		klasses.put(t, res);
		redefineSubtypes(oldkls, res, vals, done);
		return res;
	}
	
	private void deloldkls(LispType oldt) {
		if(subtypes.get(oldt) == null) {
			return;
		}
		
		for(LispType e : subtypes.get(oldt)) {
			Set<LispType> s = redefed.get(e);
			
			if(s == null) {
				s = new HashSet<LispType>();
				redefed.put(e, s);
			}
			s.add(oldt);
			
			deloldkls(e);
		}
	}
	
	
	public LispClass redefineClass(
			LispClass oldkls,
			List<LispClass> sup,
			Collection<Symbol> ins,
			Collection<Symbol> cls,
			Collection<Symbol> ecl,
			Map<Symbol, Datum> vals,
			LispObject meta) throws LispTypeException {
		LispClass res;
		
		deloldkls(oldkls.getObjectType());
		res = redefineClass1(
				oldkls, sup, ins, cls, ecl, vals, meta,
				new HashSet<LispType>());
		//redefineSubtypes(oldkls, res, vals);
		
		return res;
	}
	
}
