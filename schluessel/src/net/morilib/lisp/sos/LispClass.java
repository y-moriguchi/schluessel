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

import java.util.Collection;
import java.util.EnumMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Keyword;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Symbol;
import net.morilib.lisp.Undef;
import net.morilib.lisp.util.WeakValues;

public final class LispClass extends SlotDatum {
	
	
	private enum SlotType {
		SINSTNACE, SCLASS, SEACHCLASS
	}
	
	//
	private static final EnumMap<SlotType, Keyword> KEYWORDS;
	
	
	static {
		EnumMap<SlotType, Keyword> kw =
			new EnumMap<SlotType, Keyword>(SlotType.class);
		
		kw.put(SlotType.SINSTNACE,  Keyword.getKeyword("instance"));
		kw.put(SlotType.SCLASS,     Keyword.getKeyword("class"));
		kw.put(SlotType.SEACHCLASS, Keyword.getKeyword("each-subclass"));
		KEYWORDS = kw;
	}
	
	
	//
	private LispObject metacls;
	private LispType type;
	private Map<Symbol, SlotType> slotinfo =
		new HashMap<Symbol, SlotType>();
	private Map<Symbol, Datum> slotval =
		new HashMap<Symbol, Datum>();
	private WeakValues<LispObject> instances =
		new WeakValues<LispObject>();
	private SOS nsos;
	private int version = 0;
	private LispClass nwklass = null;
	private LispClass oldcls  = null;
	
	
	/*package*/ LispClass(SOS nsos, LispType t) {
		super(null);
		if(nsos == null || t == null) {
			throw new NullPointerException();
		}
		this.type = t;
		this.nsos = nsos;
		this.metacls = null;
		eachcls();
	}
	
	
	/*package*/ LispClass(
			String name,
			SOS nsos,
			LispType t,
			Collection<Symbol> ins,
			Collection<Symbol> cls,
			Collection<Symbol> ecl,
			LispObject meta) {
		super(name);
		
		if(nsos == null || t == null) {
			throw new NullPointerException();
		} else if(ins == null || cls == null || ecl == null) {
			throw new NullPointerException();
		} else if(meta == null) {
			throw new NullPointerException();
		}
		
		this.type = t;
		this.nsos = nsos;
		this.metacls = meta;
		
		for(Symbol e : ins) {
			slotinfo.put(e, SlotType.SINSTNACE);
		}
		for(Symbol e : cls) {
			slotinfo.put(e, SlotType.SCLASS);
			slotval.put(e, Undef.UNDEF);
		}
		for(Symbol e : ecl) {
			slotinfo.put(e, SlotType.SEACHCLASS);
			slotval.put(e, Undef.UNDEF);
		}
		eachcls();
	}
	
	
	private void eachcls() {
		Set<Symbol> cl = new HashSet<Symbol>();
		List<LispType> cpl = type.getCPL();
		
		for(LispType t : cpl) {
			Map<Symbol, SlotType> inf;
			
			if(t.equals(type)) {
				inf = slotinfo;
			} else {
				inf = nsos.getLispClass(t).slotinfo;
			}
			
			for(Map.Entry<Symbol, SlotType> e : inf.entrySet()) {
				if(e.getValue() == SlotType.SEACHCLASS &&
						!cl.contains(e.getKey())) {
					if(t.equals(type)) {
						slotinfo.put(e.getKey(), SlotType.SEACHCLASS);
					}
					slotval.put (e.getKey(), Undef.UNDEF);
				} else if(e.getValue() == SlotType.SCLASS ||
						e.getValue() == SlotType.SINSTNACE) {
					cl.add(e.getKey());
				}
			}
		}
	}
	
	
	public LispType getObjectType() {
		return type;
	}
	
	
	/*package*/ LispObject getMetaClass() {
		if(nsos.cClass == null) {
			throw new IllegalStateException();
		} else if(metacls == null) {
			metacls = nsos.cClass.instantiate();
			//return nsos.cClass;
		}
		return metacls;
	}
	
	
	public LispType getType() {
		return getMetaClass().getType();
	}
	
	
	/*package*/ void refreshClass() {
		if(nwklass != null) {
			this.metacls   = nwklass.metacls;
			this.slotinfo  = nwklass.slotinfo;
			this.slotval   = nwklass.slotval;
			this.nsos      = nwklass.nsos;
			this.type      = nwklass.type;
			this.instances = nwklass.instances;
			this.version   = nwklass.version;
			nwklass = null;
			//oldcls = null;
		}
	}
	
	
	/*package*/ void reinstantiateAll() {
		for(LispObject iss : instances.values()) {
			iss.reinstantiate();
		}
	}
	
	
	/*package*/ Datum getSlot0(Symbol name) {
		SlotType t;
		
		refreshClass();
		t = slotinfo.get(name);
		if(t == SlotType.SINSTNACE) {
			return null;
		} else if(t == SlotType.SCLASS || t == SlotType.SEACHCLASS) {
			return slotval.get(name);
		} else {
			List<LispType> cpl = type.getCPL();
			
			for(LispType e : cpl) {
				if(!e.equals(type)) {
					LispClass k = nsos.getLispClass(e);
					SlotType t2 = k.slotinfo.get(name);
					
					if(t2 == SlotType.SCLASS) {
						Datum v2 = k.slotval.get(name);
						
						if(v2 != null) {
							return v2;
						}
					} else if(t2 == SlotType.SEACHCLASS) {
						Datum v2 = slotval.get(name);
						
						if(v2 != null) {
							return v2;
						}
					} else if(t2 != null) {
						return null;
					}
				}
			}
			return null;
		}
	}
	
	
	public Datum getSlot(Symbol name) {
		Datum res = getSlot0(name);
		
		return (res == null) ? metacls.getSlot(name) : res;
	}
	
	
	/*package*/ boolean setSlot0(Symbol name, Datum val) {
		SlotType t;
		
		refreshClass();
		t = slotinfo.get(name);
		if(t == SlotType.SINSTNACE) {
			return false;
		} else if(t == SlotType.SCLASS || t == SlotType.SEACHCLASS) {
			slotval.put(name, val);
			return true;
		} else {
			List<LispType> cpl = type.getCPL();
			
			for(LispType e : cpl) {
				if(!e.equals(type)) {
					LispClass k = nsos.getLispClass(e);
					SlotType t2 = k.slotinfo.get(name);
					
					if(t2 == SlotType.SCLASS) {
						if(k.slotval.containsKey(name)) {
							k.slotval.put(name, val);
							return true;
						}
					} else if(t2 == SlotType.SEACHCLASS) {
						if(slotval.containsKey(name)) {
							slotval.put(name, val);
							return true;
						}
					} else if(t2 != null) {
						return false;
					}
				}
			}
			return false;
		}
	}
	
	
	public boolean setSlot(Symbol name, Datum val, LispMessage mesg) {
		return setSlot0(name, val) ?
				true : metacls.setSlot(name, val, mesg);
	}
	
	
	public Keyword getSlotKeyword(Symbol name) {
		refreshClass();
		SlotType t = slotinfo.get(name);
		
		if(t != null) {
			return KEYWORDS.get(t);
		} else {
			List<LispType> cpl = type.getCPL();
			
			for(LispType e : cpl) {
				if(!e.equals(type)) {
					LispClass k = nsos.getLispClass(e);
					SlotType t2 = k.slotinfo.get(name);
					
					if(t2 != null) {
						return KEYWORDS.get(t2);
					}
				}
			}
			return null;
		}
	}
	
	
	private Collection<Symbol> getSlotTypeSlots(SlotType sltp) {
		refreshClass();
		Set<Symbol> res = new HashSet<Symbol>();
		
		for(Map.Entry<Symbol, SlotType> e : slotinfo.entrySet()) {
			if(e.getValue() == sltp) {
				res.add(e.getKey());
			}
		}
		return res;
	}
	
	
	public Collection<Symbol> getInstanceSlots() {
		return getSlotTypeSlots(SlotType.SINSTNACE);
	}
	
	
	public Collection<Symbol> getClassSlots() {
		return getSlotTypeSlots(SlotType.SCLASS);
	}
	
	
	public Collection<Symbol> getEachClassSlots() {
		return getSlotTypeSlots(SlotType.SEACHCLASS);
	}
	
	
	/*package*/ LispObject instantiate() {
		refreshClass();
		Map<Symbol, Datum> in = new HashMap<Symbol, Datum>();
		Set<Symbol> cl = new HashSet<Symbol>();
		List<LispType> cpl = type.getCPL();
		LispObject res;
		
		for(LispType t : cpl) {
			Map<Symbol, SlotType> inf = nsos.getLispClass(t).slotinfo;
			
			for(Map.Entry<Symbol, SlotType> e : inf.entrySet()) {
				if(e.getValue() == SlotType.SINSTNACE &&
						!cl.contains(e.getKey())) {
					in.put(e.getKey(), Undef.UNDEF);
				} else if(e.getValue() == SlotType.SCLASS ||
						e.getValue() == SlotType.SEACHCLASS) {
					cl.add(e.getKey());
				}
			}
		}
		
		instances.add(res = new LispObject(this, in));
		return res;
	}
	
	// redefine -------------------------------
	private void eachclsRedef(LispClass rdf) {
		Set<Symbol> cl = new HashSet<Symbol>();
		List<LispType> cpl = rdf.type.getCPL();
		
		for(LispType t : cpl) {
			Map<Symbol, SlotType> inf;
			
			if(t.equals(rdf.type)) {
				inf = rdf.slotinfo;
			} else {
				inf = rdf.nsos.getLispClass(t).slotinfo;
			}
			
			for(Map.Entry<Symbol, SlotType> e : inf.entrySet()) {
				if(e.getValue() == SlotType.SEACHCLASS &&
						!cl.contains(e.getKey())) {
					Datum r2 = slotval.get(e.getKey());
					
					//rdf.slotinfo.put(e.getKey(), SlotType.SEACHCLASS);
					if(slotinfo.get(e.getKey()) != SlotType.SCLASS &&
							r2 != null) {
						rdf.slotval.put(e.getKey(), r2);
					} else {
						rdf.slotval.put(e.getKey(), Undef.UNDEF);
					}
				} else if(e.getValue() == SlotType.SCLASS &&
						t.equals(rdf.type)) {
					if(slotinfo.get(e.getKey()) == SlotType.SCLASS) {
						rdf.slotval.put(
								e.getKey(), slotval.get(e.getKey()));
					} else {
						rdf.slotval.put(e.getKey(), Undef.UNDEF);
					}
					cl.add(e.getKey());
				} else if(e.getValue() == SlotType.SINSTNACE) {
					cl.add(e.getKey());
				}
			}
		}
	}
	
	
	/*package*/ LispClass redefine(
			LispType t,
			Collection<Symbol> ins,
			Collection<Symbol> cls,
			Collection<Symbol> ecl,
			Map<Symbol, Datum>  vals,
			LispObject meta) {
		LispClass nwc =
			new LispClass(getName(), nsos, t, ins, cls, ecl, meta);
		
		eachclsRedef(nwc);
		
		for(Symbol e : cls) {
			if(slotinfo.get(e) == SlotType.SCLASS) {
				nwc.slotval.put(e, slotval.get(e));
			} else if(vals.get(e) != null) {
				nwc.slotval.put(e, vals.get(e));
			}
		}
		
		for(Symbol e : ecl) {
			if(slotinfo.get(e) == SlotType.SEACHCLASS) {
				nwc.slotval.put(e, slotval.get(e));
			} else if(vals.get(e) != null) {
				nwc.slotval.put(e, vals.get(e));
			}
		}
		
		for(LispObject iss : instances.values()) {
			iss.setNewClass(nwc, vals);
		}
		nwc.instances = instances;
		nwc.version++;
		
		if(oldcls == null) {
			nwklass = nwc;
		} else {
			oldcls.nwklass = nwc;
		}
		nwc.oldcls = this;
		
		return nwc;
	}
	
	
	private boolean isslotinfo(LispClass old, Symbol s) {
		Set<Symbol> cl = new HashSet<Symbol>();
		List<LispType> cpl = old.type.getCPL();
		
		for(LispType t : cpl) {
			Map<Symbol, SlotType> inf = old.nsos.getLispClass(t).slotinfo;
			
			for(Map.Entry<Symbol, SlotType> e : inf.entrySet()) {
				if(s.equals(e.getKey()) &&
						e.getValue() == SlotType.SINSTNACE &&
						!cl.contains(e.getKey())) {
					return true;
				} else if(e.getValue() == SlotType.SCLASS ||
						e.getValue() == SlotType.SEACHCLASS) {
					cl.add(e.getKey());
				}
			}
		}
		return false;
	}
	
	/*package*/ Map<Symbol, Datum> reinstantiate(
			Map<Symbol, Datum> oin,
			Map<Symbol, Datum> vals,
			LispClass old) {
		Map<Symbol, Datum> in = new HashMap<Symbol, Datum>();
		Set<Symbol> cl = new HashSet<Symbol>();
		List<LispType> cpl = type.getCPL();
		
		for(LispType t : cpl) {
			Map<Symbol, SlotType> inf = nsos.getLispClass(t).slotinfo;
			
			for(Map.Entry<Symbol, SlotType> e : inf.entrySet()) {
				if(e.getValue() == SlotType.SINSTNACE &&
						!cl.contains(e.getKey())) {
					//if(old.slotinfo.get(e.getKey())
					//		== SlotType.SINSTNACE) {
					if(isslotinfo(old, e.getKey())) {
						in.put(e.getKey(), oin.get(e.getKey()));
					} else if(vals.get(e.getKey()) != null) {
						in.put(e.getKey(), vals.get(e.getKey()));
					} else {
						in.put(e.getKey(), Undef.UNDEF);
					}
				} else if(e.getValue() == SlotType.SCLASS ||
						e.getValue() == SlotType.SEACHCLASS) {
					cl.add(e.getKey());
				}
			}
		}
		return in;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.NamableDatum#display()
	 */
	@Override
	public String display() {
		if(version > 0) {
			//return ("#<class " + printName() +
			//		" version(" + version + ")>");
			return "#<class " + printName() + ">";
		} else {
			return "#<class " + printName() + ">";
		}
	}
	
}
