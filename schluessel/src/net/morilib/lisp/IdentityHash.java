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

import java.util.Collection;
import java.util.Collections;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.Set;

import net.morilib.lisp.subr.BinaryArgs;
import net.morilib.lisp.subr.TernaryArgs;
import net.morilib.lisp.subr.UnaryArgs;

public class IdentityHash extends Datum implements Map<Datum, Datum> {
	
	//
	private Map<Datum, Datum> hash =
		new IdentityHashMap<Datum, Datum>();
	
	
	public static class ClearS extends UnaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Environment env, LispMessage mesg) {
			if(c1a instanceof IdentityHash) {
				((IdentityHash)c1a).hash.clear();
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.identityhash.required", c1a);
			}
		}
		
	}
	
	
	public static class DeleteS extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof IdentityHash) {
				((IdentityHash)c1a).hash.remove(c2a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.identityhash.required", c1a);
			}
		}
		
	}
	
	
	public static class IsExists extends BinaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Environment env,
				LispMessage mesg) {
			if(c1a instanceof IdentityHash) {
				boolean res = ((IdentityHash)c1a).hash.containsKey(c2a);
				
				return LispBoolean.getInstance(res);
			} else {
				throw mesg.getError("err.identityhash.required", c1a);
			}
		}
		
	}
	
	
	public static class Get extends Subr {
		
		private Datum exec(
				Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof IdentityHash) {
				IdentityHash hs = (IdentityHash)c1a;
				
				if(hs.hash.containsKey(c2a)) {
					return hs.hash.get(c2a);
				} else if(c3a != null) {
					return c3a;
				} else {
					throw mesg.getError("err.hash.valuenotfound", c2a);
				}
			} else {
				throw mesg.getError("err.identityhash.required", c1a);
			}
		}
		
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(body instanceof Cons) {
				Cons c1 = (Cons)body;
				Datum c1a = c1.getCar();
				
				if(c1.getCdr() instanceof Cons) {
					Cons c2 = (Cons)c1.getCdr();
					Datum c2a = c2.getCar();
					Datum c3a = null;
					
					if(c2.getCdr() instanceof Cons) {
						Cons c3 = (Cons)c2.getCdr();
						
						c3a = c3.getCar();
						if(c3.getCdr() != Nil.NIL) {
							throw mesg.getError(
									"err.argument", symbolName);
						}
					} else if(c2.getCdr() != Nil.NIL) {
						throw mesg.getError("err.argument", symbolName);
					}
					return exec(c1a, c2a, c3a, env, mesg);
				}
			}
			throw mesg.getError("err.argument", symbolName);
		}
		
	}
	
	
	public static class PutS extends TernaryArgs {

		@Override
		protected Datum execute(
				Datum c1a, Datum c2a, Datum c3a,
				Environment env, LispMessage mesg) {
			if(c1a instanceof IdentityHash) {
				((IdentityHash)c1a).hash.put(c2a, c3a);
				return Undef.UNDEF;
			} else {
				throw mesg.getError("err.identityhash.required", c1a);
			}
		}
		
	}
	
	
	public static class Make extends Subr {

		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			if(body == Nil.NIL) {
				return new IdentityHash();
			} else {
				throw mesg.getError("err.argument", symbolName);
			}
		}
		
	}
	
	
	public void clear() {
		throw new UnsupportedOperationException();
	}

	
	public boolean containsKey(Object key) {
		return hash.containsKey(key);
	}

	
	public boolean containsValue(Object value) {
		return hash.containsValue(value);
	}

	
	public Set<Map.Entry<Datum, Datum>> entrySet() {
		return Collections.unmodifiableSet(hash.entrySet());
	}

	
	public Datum get(Object key) {
		return hash.get(key);
	}

	
	public boolean isEmpty() {
		return hash.isEmpty();
	}

	
	public Set<Datum> keySet() {
		return Collections.unmodifiableSet(hash.keySet());
	}

	
	public Datum put(Datum key, Datum value) {
		throw new UnsupportedOperationException();
	}

	
	public void putAll(Map<? extends Datum, ? extends Datum> t) {
		throw new UnsupportedOperationException();
	}

	
	public Datum remove(Object key) {
		throw new UnsupportedOperationException();
	}

	
	public int size() {
		return hash.size();
	}

	
	public Collection<Datum> values() {
		return Collections.unmodifiableCollection(hash.values());
	}
	
}
