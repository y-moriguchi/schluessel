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

import java.util.List;

public class SubrJavaNew extends Subr {

	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		if(body instanceof Cons) {
			Datum bcar = ((Cons)body).getCar();
			Datum bcdr = ((Cons)body).getCdr();
			
			if(bcar instanceof JavaClass) {
				List<Datum> lst = LispUtils.consToList(bcdr, mesg);
				
				try {
					JavaInstance ji;
					
					ji = ((JavaClass)bcar).newJavaInstance(lst);
					return ji;
				} catch (ParameterNotFoundException e) {
					throw mesg.getError(
							"err.java.constractor.notfound", bcar);
				}
			} else {
				throw mesg.getError("err.require.java-class", bcar);
			}
		} else {
			throw mesg.getError("err.argument", "java-new");
		}
	}

}
