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

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import net.morilib.lisp.r6rs.LibraryID;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
/*package*/ class CompiledCode {

	/*package*/ static enum Oper {
		PUSH,
		POP,
		BEGIN_LIST,
		APPEND_LIST,
		APPEND_LIST_SPLICING,
		APPEND_LIST_MULTI_VALUES,
		END_LIST_DOT,
		END_LIST,
		END_LIST_VECTOR,
		CALL,
		CALL_WIND,
		CALL_METHOD,
		CALL_TAIL,
		JMP,
		JMP_IF,
		JMP_UNLESS,
		JMP_TOP,
		REFER_SYMBOL,
		REFER_SETTER,
		REFER_DEFINED,
		BIND,
		BIND_MACRO,
		BIND_MACRO72,
		BIND_MACRO_QUOTE,
		SET,
		RETURN_OP,
		PUSH_CONTINUATION,
		PUSH_USYN,
		POP_USYN,
		FORCE,
		NEW_PROMISE,
		APPEND_VALUES,
		EVAL_CODE,
		LOAD_CODE,
		OVERRIDE_LOAD_CODE,
		EXPAND_MACRO,
		EXPAND_MACRO1,
		EXPAND_MACRO72,
		FIND_LIST,
		BIND_METHOD,
		SET_LIBID,
		IMPORT_LIB,
		EXPAND_SYNTAX_RULE,
		REPLACE_PARAM,
		JAVA_CALL,
		JAVA_GET,
		JAVA_SET,
		JAVA_FIELD_GET,
		JAVA_FIELD_SET,
		JAVA_INSTANCEOF,
		JAVA_RAISE,
		JAVA_ENTER_EXCEPTION_HANDLER,
		JAVA_LEAVE_EXCEPTION_HANDLER,
		REWIND_ENV,
		REWIND_CONT,
		ENTER_EXCEPTION_HANDLER,
		LEAVE_EXCEPTION_HANDLER,
		GET_CURRENT_EXCEPTION_HANDLER,
		PARAMETERIZE,
		DEPARAMETERIZE,
		INITIALLY,
		FINALLY,
	};

	/*package*/ static final Code CALL_CD = new Code(Oper.CALL);

	/*package*/ static class Code {

		private Code(Oper op) {
			this.op = op;
		}

		private Oper op;
		private Datum datum;
		private int jmpLabel;
		private UserSyntax usyn;
		private SymbolName method;
		private LispSmallInt beanIndex;
		private int rewind;
		private LibraryID libid;
		private Map<Symbol, Symbol> export;
		private UserSyntax syntax;
		private int depth;
		private CompiledCode codes;

		/*package*/ Oper getOp() {
			return op;
		}

		/*package*/ Datum getDatum() {
			return datum;
		}

		/*package*/ int getJmpLabel() {
			return jmpLabel;
		}

		/*package*/ UserSyntax getUsyn() {
			return usyn;
		}

		/*package*/ SymbolName getMethod() {
			return method;
		}

		/*package*/ LispSmallInt getBeanIndex() {
			return beanIndex;
		}

		/*package*/ int getRewind() {
			return rewind;
		}

		/*package*/ LibraryID getLibID() {
			return libid;
		}

		/*package*/ Map<Symbol, Symbol> getExport() {
			return export;
		}

		/*package*/ UserSyntax getSyntax() {
			return syntax;
		}

		/*package*/ int getDepth() {
			return depth;
		}

		/*package*/ CompiledCode getCodes() {
			return codes;
		}

		public String toString() {
			return op + ":" + datum + ":" + jmpLabel;
		}

	}

	private List<Code> opers = new ArrayList<Code>();
	private List<Integer> labels = new ArrayList<Integer>();


	/*package*/ static class Builder {

		private CompiledCode codes = new CompiledCode();

		//
		/*package*/ Builder() {
			// do nothing
		}

		//
		/*package*/ void addPush(Datum d) {
			Code c = new Code(Oper.PUSH);

			c.datum = d;
			codes.opers.add(c);
		}

		//
		/*package*/ void addPop() {
			codes.opers.add(new Code(Oper.POP));
		}

		//
		/*package*/ void addBeginList() {
			codes.opers.add(new Code(Oper.BEGIN_LIST));
		}

		//
		/*package*/ void addAppendList() {
			codes.opers.add(new Code(Oper.APPEND_LIST));
		}

		//
		/*package*/ void addAppendListSplicing() {
			codes.opers.add(new Code(Oper.APPEND_LIST_SPLICING));
		}

		//
		/*package*/ void addAppendListMultiValues() {
			codes.opers.add(new Code(Oper.APPEND_LIST_MULTI_VALUES));
		}

		//
		/*package*/ void addEndListDot() {
			codes.opers.add(new Code(Oper.END_LIST_DOT));
		}

		//
		/*package*/ void addEndList() {
			codes.opers.add(new Code(Oper.END_LIST));
		}

		//
		/*package*/ void addEndListVector() {
			codes.opers.add(new Code(Oper.END_LIST_VECTOR));
		}

		//
		/*package*/ void addCall() {
			codes.opers.add(new Code(Oper.CALL));
		}

		//
		/*package*/ void addCallWind() {
			codes.opers.add(new Code(Oper.CALL_WIND));
		}

		//
		/*package*/ void addCallMethod() {
			codes.opers.add(new Code(Oper.CALL_METHOD));
		}

		//
		/*package*/ void addCallTail(int rewind) {
			Code c = new Code(Oper.CALL_TAIL);

			c.rewind = rewind;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJmp(int jmpLabel) {
			Code c = new Code(Oper.JMP);

			c.jmpLabel = jmpLabel;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJmpIf(int jmpLabel) {
			Code c = new Code(Oper.JMP_IF);

			c.jmpLabel = jmpLabel;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJmpUnless(int jmpLabel) {
			Code c = new Code(Oper.JMP_UNLESS);

			c.jmpLabel = jmpLabel;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJmpTop() {
			codes.opers.add(new Code(Oper.JMP_TOP));
		}

		//
		/*package*/ void addReferSymbol(Datum destSymbol) {
			Code c = new Code(Oper.REFER_SYMBOL);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addReferSetter(Datum destSymbol) {
			Code c = new Code(Oper.REFER_SETTER);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addDefined(Datum destSymbol) {
			Code c = new Code(Oper.REFER_DEFINED);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addBind(Datum destSymbol) {
			Code c = new Code(Oper.BIND);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addBindMacro(Datum destSymbol) {
			Code c = new Code(Oper.BIND_MACRO);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addBindMacro72(Datum destSymbol) {
			Code c = new Code(Oper.BIND_MACRO72);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*pacakge*/ void addBindMacroQuote(Datum destSymbol) {
			Code c = new Code(Oper.BIND_MACRO_QUOTE);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addSet(Datum destSymbol) {
			Code c = new Code(Oper.SET);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addReturnOp() {
			codes.opers.add(new Code(Oper.RETURN_OP));
		}

		//
		/*package*/ void addPushContinuation() {
			Code c = new Code(Oper.PUSH_CONTINUATION);

			//c.jmpLabel = jmplabel;
			codes.opers.add(c);
		}

		//
		/*package*/ void addPushUsyn(UserSyntax usyn) {
			Code c = new Code(Oper.PUSH_USYN);

			c.usyn = usyn;
			codes.opers.add(c);
		}

		//
		/*package*/ void addPopUsyn() {
			codes.opers.add(new Code(Oper.POP_USYN));
		}

		//
		/*package*/ void addForce() {
			codes.opers.add(new Code(Oper.FORCE));
		}

		//
		/*package*/ void addNewPromise(Datum p) {
			Code c = new Code(Oper.NEW_PROMISE);

			c.datum = p;
			codes.opers.add(c);
		}

		//
		/*package*/ void addAppendValues() {
			codes.opers.add(new Code(Oper.APPEND_VALUES));
		}

		//
		/*package*/ void addEvalCode() {
			codes.opers.add(new Code(Oper.EVAL_CODE));
		}

		//
		/*package*/ void addLoadCode() {
			codes.opers.add(new Code(Oper.LOAD_CODE));
		}

		//
		/*package*/ void addOverrideLoadCode() {
			codes.opers.add(new Code(Oper.OVERRIDE_LOAD_CODE));
		}

		//
		/*package*/ void addExpandMacro() {
			codes.opers.add(new Code(Oper.EXPAND_MACRO));
		}

		//
		/*package*/ void addExpandMacro1() {
			codes.opers.add(new Code(Oper.EXPAND_MACRO1));
		}

		//
		/*package*/ void addExpandMacro72() {
			codes.opers.add(new Code(Oper.EXPAND_MACRO72));
		}

		//
		/*package*/ void addFindList() {
			codes.opers.add(new Code(Oper.FIND_LIST));
		}

		//
		/*package*/ void addBindMethod(Datum destSymbol) {
			Code c = new Code(Oper.BIND_METHOD);

			c.datum = destSymbol;
			codes.opers.add(c);
		}

		//
		/*package*/ void addSetLibID(
				LibraryID libid, Map<Symbol, Symbol> mp) {
			Code c = new Code(Oper.SET_LIBID);

			c.libid = libid;
			c.export = mp;
			codes.opers.add(c);
		}

		//
		/*package*/ void addImportLib(Datum mp) {
			Code c = new Code(Oper.IMPORT_LIB);

			c.datum = mp;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaCall(
				SymbolName method, Datum callable) {
			Code c = new Code(Oper.JAVA_CALL);

			c.datum = callable;
			c.method = method;
			codes.opers.add(c);
		}

		//
		/*package*/ void addExpandSyntaxRule(UserSyntax syn) {
			Code c = new Code(Oper.EXPAND_SYNTAX_RULE);

			c.syntax = syn;
			codes.opers.add(c);
		}

		//
		/*package*/ void addReplaceParam() {
			codes.opers.add(new Code(Oper.REPLACE_PARAM));
		}

		//
		/*package*/ void addJavaGet(
				SymbolName accessor, Datum instance, LispSmallInt ind) {
			Code c = new Code(Oper.JAVA_GET);

			c.datum = instance;
			c.method = accessor;
			c.beanIndex = ind;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaSet(
				SymbolName accessor, Datum instance, LispSmallInt ind) {
			Code c = new Code(Oper.JAVA_SET);

			c.datum = instance;
			c.method = accessor;
			c.beanIndex = ind;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaFieldGet(
				SymbolName accessor, Datum instance) {
			Code c = new Code(Oper.JAVA_FIELD_GET);

			c.datum = instance;
			c.method = accessor;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaFieldSet(
				SymbolName accessor, Datum instance) {
			Code c = new Code(Oper.JAVA_FIELD_SET);

			c.datum = instance;
			c.method = accessor;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaInstanceof(SymbolName name) {
			Code c = new Code(Oper.JAVA_INSTANCEOF);

			c.method = name;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaRaise() {
			codes.opers.add(new Code(Oper.JAVA_RAISE));
		}

		//
		/*package*/ void addJavaEnterExceptionHandler(
				Datum callee, int endjmp) {
			Code c = new Code(Oper.JAVA_ENTER_EXCEPTION_HANDLER);

			c.datum = callee;
			c.jmpLabel = endjmp;
			codes.opers.add(c);
		}

		//
		/*package*/ void addJavaLeaveExceptionHandler() {
			codes.opers.add(new Code(
					Oper.JAVA_LEAVE_EXCEPTION_HANDLER));
		}

		//
		/*package*/ void addRewindEnv(int r) {
			Code c = new Code(Oper.REWIND_ENV);

			c.rewind = r;
			codes.opers.add(c);
		}

		//
		/*package*/ void addRewindCont(int r) {
			Code c = new Code(Oper.REWIND_CONT);

			c.rewind = r;
			codes.opers.add(c);
		}

		//
		/*package*/ void addEnterExceptionHandler(
				int endjmp) {
			Code c = new Code(Oper.ENTER_EXCEPTION_HANDLER);

			c.jmpLabel = endjmp;
			codes.opers.add(c);
		}

		//
		/*package*/ void addLeaveExceptionHandler() {
			codes.opers.add(new Code(
					Oper.LEAVE_EXCEPTION_HANDLER));
		}

		//
		/*package*/ void addGetCurrentExceptionHandler() {
			codes.opers.add(new Code(
					Oper.GET_CURRENT_EXCEPTION_HANDLER));
		}

		//
		/*package*/ void addParameterize() {
			codes.opers.add(new Code(Oper.PARAMETERIZE));
		}

		//
		/*package*/ void addDeparameterize() {
			codes.opers.add(new Code(Oper.DEPARAMETERIZE));
		}

		//
		/*package*/ void addInitially(CompiledCode codes) {
			Code c = new Code(Oper.INITIALLY);

			c.codes = codes;
			this.codes.opers.add(c);
		}

		//
		/*package*/ void addFinally(CompiledCode codes) {
			Code c = new Code(Oper.FINALLY);

			c.codes = codes;
			this.codes.opers.add(c);
		}

		//
		/*package*/ int getCurrentAddress() {
			return codes.opers.size();
		}

		//
		/*package*/ void setCurrentAddressToLabel(int label) {
			codes.labels.set(label, getCurrentAddress());
		}

		//
		/*package*/ int allocLabel() {
			int res = codes.labels.size();

			codes.labels.add(-1);
			return res;
		}

		//
		/*package*/ CompiledCode getCodeRef() {
			return codes;
		}

		//
		/*pacakge*/ void merge(CompiledCode cc) {
			codes.opers.addAll(cc.opers);
			codes.labels.addAll(cc.labels);
		}

	}

	//
	/*package*/ Code getCode(int addr) {
		return addr < opers.size() ? opers.get(addr) : null;
	}

	//
	/*package*/ int getAddress(int label) {
		return labels.get(label);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		StringBuilder buf = new StringBuilder();

		for(int i = 0; i < opers.size(); i++) {
			Code c = opers.get(i);

			if(c.datum instanceof ClosureClass) {
				buf.append("----------------------------\n");
				buf.append(((ClosureClass)c.datum).getCode());
				buf.append("----------------------------\n");
			} else if(c.datum instanceof Closure) {
				buf.append("----------------------------\n");
				buf.append(((Closure)c.datum).getCode());
				buf.append("----------------------------\n");
			}

			buf.append(c.toString());
			buf.append("\n");
		}
		return buf.toString();
	}

}
