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
package net.morilib.lang.proposition;

import java.util.Iterator;
import java.util.List;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/11/14
 */
public final class Logical {
	
	//
	private Logical() {}
	
	/**
	 * 
	 * @param <S>
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static<S extends Proposition<S>> Proposition<S> getTrue() {
		return (Proposition<S>)TrueFalseProposition.TRUE;
	}
	
	/**
	 * 
	 * @param <S>
	 * @return
	 */
	@SuppressWarnings("unchecked")
	public static<S extends Proposition<S>> Proposition<S> getFalse() {
		return (Proposition<S>)TrueFalseProposition.FALSE;
	}
	
	
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public static boolean isIndependent(
			Proposition... propositions) {
		if(propositions.length < 2) {
			return true;
		} else {
			Proposition p = propositions[0];
			
			for(int i = 1; i < propositions.length; i++) {
				if(!p.isIndependent(propositions[i])) {
					return false;
				}
			}
			return true;
		}
	}
	
	
	public static<S extends Proposition<S>> boolean isIndependent(
			List<Proposition<S>> propositions) {
		if(propositions.size() < 2) {
			return true;
		} else {
			Iterator<Proposition<S>> it = propositions.iterator();
			Proposition<S> p = it.next();
			
			while(it.hasNext()) {
				if(!p.isIndependent(it.next())) {
					return false;
				}
			}
			return true;
		}
	}
	
}
