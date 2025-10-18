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

import java.beans.IntrospectionException;
import java.util.List;

import net.morilib.lisp.sos.ISlotDatum;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public class JavaInstance extends Datum
implements JavaObjective, ISlotDatum {

	//
	private Object instance;

	//
	/*package*/ JavaInstance(Object instance) {
		this.instance = instance;
	}

	//
	/*package*/ Object getJavaInstance() {
		return instance;
	}

	//
	/*package*/ Datum invokeMethod(
			String name, List<Datum> lst
			) throws ParameterNotFoundException {
		Object res;

		res = JavaUtils.invokeMethod(
				instance.getClass(), instance, name, lst);
		return LispUtils.toDatum(res);
	}

	//
	/*package*/ Datum invokeGetter(String name
			) throws IntrospectionException, ParameterNotFoundException {
		Object res;

		res = JavaUtils.invokeGetter(instance, name);
		return LispUtils.toDatum(res);
	}

	//
	/*package*/ void invokeSetter(String name, Datum d
			) throws IntrospectionException, ParameterNotFoundException {
		JavaUtils.invokeSetter(instance, name, d);
	}

	//
	/*package*/ Datum invokeGetter(String name, int index
			) throws IntrospectionException, ParameterNotFoundException {
		Object res;

		res = JavaUtils.invokeGetter(instance, name, index);
		return LispUtils.toDatum(res);
	}

	//
	/*package*/ void invokeSetter(String name, int index, Datum d
			) throws IntrospectionException, ParameterNotFoundException {
		JavaUtils.invokeSetter(instance, name, index, d);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	public Object toObject() {
		return instance;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#getSlot(net.morilib.lisp.Symbol)
	 */
	public Datum getSlot(Symbol sym) {
		try {
			return LispJavaUtils.newInstance(JavaUtils.invokeGetter(
					instance, sym.getName()));
		} catch (IntrospectionException e) {
			return null;
		} catch (ParameterNotFoundException e) {
			return null;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.sos.ISlotDatum#setSlot(net.morilib.lisp.Symbol, net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	public boolean setSlot(Symbol sym, Datum val, LispMessage mesg) {
		try {
			JavaUtils.invokeSetter(instance, sym.getName(), val);
			return true;
		} catch (IntrospectionException e) {
			return false;
		} catch (ParameterNotFoundException e) {
			return false;
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		Object o = getJavaInstance();

		buf.append("#<java-instance ");
		buf.append(o.getClass().getName());
		buf.append(">");
	}

}
