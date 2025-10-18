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
import java.util.Set;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/15
 */
public final class UserSyntax extends Datum
implements ExecuteEnvScope {

	//
	private String name;                  // シンタックス名
	private List<Datum> patternList;      // パターンのリスト
	private List<Datum> templateList;     // テンプレートのリスト
	private List<Set<Symbol>> paramList;  // パラメータのリスト
	private Set<Symbol> reservedSet;      // 予約語のリスト
	private Environment compileEnv;       // コンパイル時の環境
	private boolean definedLetSyntax;    // let-syntaxで定義されたか
	private CompiledCode lambda;          // lambda expr
	private UserSyntax rootsyn;

	// 実行時の環境(let-syntaxで定義されたときのみ使用)
	private Environment executeEnv = null;

	//
	/*package*/ UserSyntax(
			String name,
			List<Datum> patlist,
			List<Datum> tmplist,
			List<Set<Symbol>> paramList,
			Set<Symbol> reserved,
			Environment compileEnv,
			boolean definedLetSyntax) {
		if(name == null || patlist == null ||
				tmplist == null || reserved == null ||
				compileEnv == null) {
			throw new NullPointerException();
		}

		this.name = name;
		this.patternList = patlist;
		this.templateList = tmplist;
		this.paramList = paramList;
		this.reservedSet = reserved;
		this.compileEnv = compileEnv;
		this.definedLetSyntax = definedLetSyntax;
		this.lambda = null;
	}

	//
	/*package*/ UserSyntax(
			String name,
			List<Datum> patlist,
			List<Datum> tmplist,
			List<Set<Symbol>> paramList,
			Set<Symbol> reserved,
			Environment compileEnv,
			UserSyntax rootsyn) {
		if(name == null || patlist == null ||
				tmplist == null || reserved == null ||
				compileEnv == null
				) {
			throw new NullPointerException();
		}

		this.name = name;
		this.patternList = patlist;
		this.templateList = tmplist;
		this.paramList = paramList;
		this.reservedSet = reserved;
		this.compileEnv = compileEnv;
		this.rootsyn = rootsyn;
		this.lambda = null;
	}

	//
	/*package*/ UserSyntax(
			String name,
			Environment compileEnv,
			CompiledCode lambda) {
		if(name == null || lambda == null || compileEnv == null) {
			throw new NullPointerException();
		}

		this.name = name;
		this.lambda = lambda;
		this.compileEnv = compileEnv;
	}

	/**
	 * @return the patternList
	 */
	public List<Datum> getPatternList() {
		return patternList;
	}

	/**
	 * @return the templateList
	 */
	public List<Datum> getTemplateList() {
		return templateList;
	}

	/**
	 * @return the reservedSet
	 */
	public Set<Symbol> getReservedSet() {
		return reservedSet;
	}

	/**
	 * @return the name
	 */
	public String getName() {
		//return name;
		UserSyntax s2 = this;

		while(s2.getRootSyntax() != null) {
			s2 = s2.getRootSyntax();
		}
		return s2.name;
	}

	/**
	 * @return the paramList
	 */
	public List<Set<Symbol>> getParamList() {
		return paramList;
	}

	/**
	 * @return the compileEnv
	 */
	public Environment getCompileEnv() {
		return compileEnv;
	}

	/**
	 * @return the definedLetSyntax
	 */
	public boolean isDefinedLetSyntax() {
		return definedLetSyntax;
	}

	/**
	 * @return the executeEnv
	 */
	public Environment getExecuteEnv() {
		return executeEnv;
	}

	//
	/*package*/ void setExecuteEnv(Environment executeEnv) {
		this.executeEnv = executeEnv;
	}

	//
	/*package*/ CompiledCode getLambda() {
		return lambda;
	}

	//
	/*package*/ UserSyntax getRootSyntax() {
		return rootsyn;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<syntax " + getName() + ">");
	}

}
