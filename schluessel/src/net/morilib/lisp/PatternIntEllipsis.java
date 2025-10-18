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

/*package*/ class PatternIntEllipsis extends Datum {
	
	//
	private Datum list;
	private Datum cdr;
	//private List<Datum> vecrest = null;
	private boolean useVector;
	
	
	/*package*/ PatternIntEllipsis(
			Datum list, Datum cdr, boolean useVector) {
		if(list == null || cdr == null) {
			throw new NullPointerException();
		}
		this.list = list;
		this.cdr  = cdr;
		this.useVector = useVector;
	}
	
	
	/*package*/ /*PatternIntEllipsis(Datum list, List<Datum> vecrest) {
		if(list == null || vecrest == null) {
			throw new NullPointerException();
		}
		this.list      = list;
		this.vecrest   = vecrest;
		this.useVector = true;
	}*/
	
	
	/*package*/ PatternIntEllipsis(Datum list, Datum cdr) {
		this(list, cdr, false);
	}
	
	
	/*package*/ Datum getEllipsisList() {
		return list;
	}
	
	
	/*package*/ Datum getCdr() {
		return cdr;
	}
	
	
	/*package*/ /*List<Datum> getVectorRest() {
		return Collections.unmodifiableList(vecrest);
	}*/
	
	
	/*package*/ boolean isUseVector() {
		return useVector;
	}
	
	
	public String toString() {
		return list + "... . " + cdr;
	}

}
