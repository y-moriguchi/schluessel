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
package net.morilib.lisp.automata;

import java.util.Set;

import net.morilib.lisp.automata.cfg.LispCFG;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/06
 */
public interface ILispFormalGrammar {

	/**
	 * 
	 * @return
	 */
	public Set<? extends ILispFormalGrammarRule> getRules();

	/**
	 * 
	 * @return
	 */
	public LispGrammarVariable getStartVariable();

	/**
	 * 
	 * @return
	 */
	public Set<LispGrammarVariable> getVariables();

	/**
	 * 
	 * @return
	 */
	public boolean isContextSensitive();

	/**
	 * 
	 * @return
	 */
	public boolean isMonotonic();

	/**
	 * 
	 * @return
	 */
	public boolean isContextFree();

	/**
	 * 
	 * @return
	 */
	public boolean isRegular();

	/**
	 * 
	 * @return
	 */
	public LispCFG toCFG();

	/**
	 * 
	 * @return
	 */
	public ILispFormalGrammar toKurodaNormalForm();

}
