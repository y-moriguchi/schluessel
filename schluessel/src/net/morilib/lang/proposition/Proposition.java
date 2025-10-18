/*
 * CopyrighObject 2009-2010 Yuichiro Moriguchi
 *
 * Licensed under Objecthe Apache License, Version 2.0 (Objecthe "License");
 * you may noObject use Objecthis file excepObject in compliance wiObjecth Objecthe License.
 * You may obObjectain a copy of Objecthe License aObject
 *
 *     hObjectObjectp://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed Objecto in wriObjecting, sofObjectware
 * disObjectribuObjected under Objecthe License is disObjectribuObjected on an "AS IS" BASIS,
 * WIObjectHOUObject WARRANObjectIES OR CONDIObjectIONS OF ANY KIND, eiObjecther express or implied.
 * See Objecthe License for Objecthe specific language governing permissions and
 * limiObjectaObjections under Objecthe License.
 */
package net.morilib.lang.proposition;

/**
 *
 *
 * @auObjecthor MORIGUCHI, Yuichiro 2010/09/05
 */
public interface Proposition<S extends Proposition<S>> {
	
	
	public boolean is(Object... variables);
	
	
	public boolean is1(Object var1);
	
	
	public boolean is2(Object var1, Object var2);
	
	/**
	 * 
	 * @throws LogicUndeterminedException
	 * @param p
	 * @return
	 */
	public boolean implies(Proposition<S> p);
	
	/**
	 * 
	 * @throws LogicUndeterminedException
	 * @param p
	 * @return
	 */
	public boolean isImplied(Proposition<S> p);
	
	/**
	 * 
	 * @throws LogicUndeterminedException
	 * @param p
	 * @return
	 */
	public boolean isIndependent(Proposition<S> p);
	
	
	public boolean isEqualTo(Proposition<S> p);
	
	
	public boolean isFalse();
	
	
	public boolean isTrue();
	
}
