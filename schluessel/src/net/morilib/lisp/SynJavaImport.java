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

import java.beans.BeanInfo;
import java.beans.IntrospectionException;
import java.beans.Introspector;
import java.beans.MethodDescriptor;
import java.beans.PropertyDescriptor;
import java.lang.reflect.Method;
import java.util.List;

import net.morilib.lisp.LispCompiler.MiscInfo;

public class SynJavaImport extends Syntax {
	
	private void introspect(
			JavaClass cls0,
			CompiledCode.Builder build,
			String name,
			LispMessage mesg) {
		try {
			Class<?> cls = cls0.getJavaClass();
			BeanInfo inf = Introspector.getBeanInfo(cls);
			
			// method
			for(MethodDescriptor m : inf.getMethodDescriptors()) {
				Method     m0 = m.getMethod();
				String     nm = name + "-" + m0.getName();
				
				build.addPush(new JavaMethod(nm, m0));
				build.addBind(Symbol.getSymbol(nm));
			}
			
			// property
			for(PropertyDescriptor p : inf.getPropertyDescriptors()) {
				String     nm = name + "-" + p.getName();
				
				build.addPush(new JavaProperty(nm, p));
				build.addBind(Symbol.getSymbol(nm));
			}
		} catch (IntrospectionException e) {
			throw mesg.getError("err.java-import.classnotfound");
		}
		
	}
	

	/*package*/ void compile(
			Datum body,
			Environment env,
			LispCompiler comp,
			CompiledCode.Builder build,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			LispMessage mesg,
			List<Cons> symlist, CodeExecutor exec, IntStack memento, MiscInfo syncased) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			
			if(IntLispUtils.isSymbolName(bcar) &&
					bcdr instanceof Cons) {
				Datum bcdar = ((Cons)bcdr).getCar();
				Datum bcddr = ((Cons)bcdr).getCdr();
				JavaClass cls;
				
				if(!IntLispUtils.isSymbolName(bcdar) ||
						bcddr != Nil.NIL) {
					throw mesg.getError("err.java-import.malform");
				}
				
				try {
					cls = new JavaClass(((SymbolName)bcdar).getName());
					
					build.addPush(cls);
					build.addBind(bcar);
					introspect(
							cls, build,
							((SymbolName)bcar).getName(), mesg);
					build.addPush(Undef.UNDEF);
				} catch (ClassNotFoundException e) {
					throw mesg.getError(
							"err.java-import.classnotfound", bcdar);
				}
			} else {
				throw mesg.getError("err.java-import.malform");
			}
		} else {
			throw mesg.getError("err.java-import.malform");
		}
	}
	
	
	/*package*/ Datum replaceLocalVals(
			Datum body,
			Environment env,
			LispCompiler comp,
			Environment ienv, LispMessage mesg, boolean toplv, int ttype) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			
			if(bcar instanceof Symbol && bcdr instanceof Symbol) {
				return body;
			} else {
				throw mesg.getError("err.java-import.malform");
			}
		} else {
			throw mesg.getError("err.java-import.malform");
		}
	}

}
