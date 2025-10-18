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

import java.beans.IndexedPropertyDescriptor;
import java.beans.PropertyDescriptor;
import java.util.List;

/*package*/ class JavaProperty extends Subr {
	
	//
	private PropertyDescriptor propdesc;
	
	private class Setter extends Subr {
		
		private Setter(String name) {
			this.symbolName = name + "$setter";
		}
		
		@Override
		public Datum eval(
				Datum body, Environment env, LispMessage mesg) {
			List<Datum> lst = LispUtils.consToList(body, mesg);
			
			if(propdesc instanceof IndexedPropertyDescriptor) {
				if(lst.size() != 3) {
					throw mesg.getError("err.argument");
				} else if(!(lst.get(0) instanceof JavaInstance)) {
					throw mesg.getError(
							"err.require.java-bean",
							lst.get(0));
				} else if(!(lst.get(1) instanceof LispSmallInt)) {
					throw mesg.getError(
							"err.require.smallint",
							lst.get(1));
				}
				
				try {
					JavaInstance bn = (JavaInstance)lst.get(0);
					LispSmallInt si = (LispSmallInt)lst.get(1);
					int          ix = si.getExactSmallInt();
					Object       jo;
					
					jo = JavaUtils.invokeSetter(
							bn.getJavaInstance(),
							(IndexedPropertyDescriptor)propdesc,
							ix,
							lst.get(2));
					return LispUtils.toDatum(jo);
				} catch (ParameterNotFoundException e) {
					throw mesg.getError(
							"err.java.setter.notfound");
				}
			} else {
				if(lst.size() != 2) {
					throw mesg.getError("err.argument");
				} else if(!(lst.get(0) instanceof JavaInstance)) {
					throw mesg.getError(
							"err.require.java-bean",
							lst.get(0));
				}
				
				try {
					JavaInstance bn = (JavaInstance)lst.get(0);
					Object       jo;
					
					jo = JavaUtils.invokeSetter(
							bn.getJavaInstance(),
							propdesc,
							lst.get(1));
					return LispUtils.toDatum(jo);
				} catch (ParameterNotFoundException e) {
					throw mesg.getError(
							"err.java.getter.notfound");
				}
			}
		}
	}
	
	
	/*package*/ JavaProperty(String name, PropertyDescriptor pd) {
		this.symbolName = name;
		this.propdesc   = pd;
		super.setSetter(new Setter(name));
	}
	
	
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		if(propdesc instanceof IndexedPropertyDescriptor) {
			if(body instanceof Cons) {
				Cons c0 = (Cons)body;
				
				if(c0.getCdr() instanceof Cons) {
					Cons  c1 = (Cons)c0.getCdr();
					
					if(c1.getCdr() != Nil.NIL) {
						throw mesg.getError("err.argument");
					} else if(!(c0.getCar() instanceof JavaInstance)) {
						throw mesg.getError(
								"err.require.java-bean",
								c0.getCar());
					} else if(!(c1.getCar() instanceof LispSmallInt)) {
						throw mesg.getError(
								"err.require.smallint",
								c1.getCar());
					}
					
					try {
						JavaInstance bn = (JavaInstance)c0.getCar();
						LispSmallInt si = (LispSmallInt)c1.getCar();
						int          ix = si.getExactSmallInt();
						Object       jo;
						
						jo = JavaUtils.invokeGetter(
								bn.getJavaInstance(),
								(IndexedPropertyDescriptor)propdesc,
								ix);
						return LispUtils.toDatum(jo);
					} catch (ParameterNotFoundException e) {
						throw mesg.getError(
								"err.java.getter.notfound");
					}
				}
			}
			throw mesg.getError("err.argument");
		} else {
			if(body instanceof Cons) {
				Cons c0 = (Cons)body;
				
				if(c0.getCdr() != Nil.NIL) {
					throw mesg.getError("err.argument");
				} else if(!(c0.getCar() instanceof JavaInstance)) {
					throw mesg.getError(
							"err.require.java-bean",
							c0.getCar());
				}
				
				try {
					JavaInstance bn = (JavaInstance)c0.getCar();
					Object       jo;
					
					jo = JavaUtils.invokeGetter(
							bn.getJavaInstance(), propdesc);
					return LispUtils.toDatum(jo);
				} catch (ParameterNotFoundException e) {
					throw mesg.getError(
							"err.java.getter.notfound");
				}
			}
			throw mesg.getError("err.argument");
		}
	}

}
