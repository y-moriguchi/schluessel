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

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2011/05/24
 */
/*package*/ abstract class LispCompiler {

	//
	/*package*/ static class MiscInfo {

		private UserSyntax usyn = null;


		/*package*/ MiscInfo(UserSyntax usyn) {
			this.usyn = usyn;
		}


		/*package*/ UserSyntax getUserSyntax() {
			return usyn;
		}

	}

	//
	/*package*/ abstract Datum expandSyntax(
			UserSyntax syn, Datum body,
			Environment env2, boolean syncase);

	/**
	 * 
	 * @param bcdr
	 * @param env
	 * @param builder
	 * @param callsym
	 * @param symlist
	 * @param exec
	 * @param memento
	 * @param syncased
	 */
	public abstract void compileArgs(
			Datum bcdr,
			Environment env,
			CompiledCode.Builder builder,
			Cons callsym,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento, MiscInfo syncased);

	/**
	 * 
	 * @param body
	 * @param env
	 * @param builder
	 * @param toplevel
	 * @param callsym
	 * @param istail
	 * @param symlist
	 * @param exec
	 * @param memento
	 * @param syncased
	 */
	public abstract void compile(
			Datum body,
			Environment env,
			CompiledCode.Builder builder,
			boolean toplevel,
			Cons callsym,
			boolean istail,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento, MiscInfo syncased);

	/**
	 * 
	 * @param body
	 * @param env
	 * @param builder
	 * @param callsym
	 * @param istail
	 * @param symlist
	 * @param exec
	 * @param memento
	 * @param syncased
	 */
	public void compile(
			Datum body,
			Environment env,
			CompiledCode.Builder builder,
			Cons callsym,
			boolean istail,
			List<Cons> symlist,
			CodeExecutor exec,
			IntStack memento, MiscInfo syncased) {
		compile(body,
				env,
				builder,
				false,
				callsym,
				istail,
				symlist,
				exec,
				memento, syncased);
	}

	/**
	 * 
	 * @param bcdr
	 * @param env
	 * @param ienv
	 * @param ttype
	 * @return
	 */
	public abstract Datum replaceLocalValsArgs(
			Datum bcdr,
			Environment env,
			Environment ienv,
			int ttype);

	// ienvはマクロ内にあるローカル変数の環境
	// このメソッドはbodyの内部を破壊的に書き換えるので注意
	public abstract Datum replaceLocalVals(
			Datum body,
			Environment env,
			Environment ienv,
			boolean toplv,
			int ttype);

	//
	/*package*/ abstract Datum replaceLocalValsOnly(
			Datum body,
			Environment env,
			Environment ienv,
			boolean toplv,
			int ttype);

	/**
	 * 
	 * @param body
	 * @param env
	 * @param exec
	 * @param memento
	 * @return
	 */
	public abstract Datum expandMacro1(
			Datum body,
			Environment env,
			CodeExecutor exec,
			IntStack memento);

	/**
	 * 
	 * @param body
	 * @param env
	 * @param exec
	 * @param memento
	 * @return
	 */
	public abstract Datum expandMacro(
			Datum body,
			Environment env,
			CodeExecutor exec,
			IntStack memento);

}
