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
package net.morilib.lisp.automata.lr;

import java.util.LinkedList;
import java.util.List;

import net.morilib.lisp.Datum;
import net.morilib.lisp.Datum2;
import net.morilib.lisp.Environment;
import net.morilib.lisp.LispBoolean;
import net.morilib.lisp.LispCharacter;
import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.LispUtils;
import net.morilib.lisp.Nil;
import net.morilib.lisp.Procedure;
import net.morilib.lisp.Scheme;
import net.morilib.lisp.automata.ILispConfiguration;
import net.morilib.lisp.automata.cfg.LispCFGRule;
import net.morilib.util.ArrayListStack;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/26
 */
public class LispLRParser extends Datum2
implements ILispConfiguration {

	//
	private ILispLRTable table;
	private ArrayListStack<Datum> synstack, semstack;
	private Procedure action;
	private Environment env;
	private boolean error, accept;

	/**
	 * 
	 * @param table
	 * @param action
	 */
	public LispLRParser(ILispLRTable table, Procedure action,
			Environment env) {
		this.table    = table;
		this.action   = action;
		this.synstack = new ArrayListStack<Datum>();
		this.semstack = new ArrayListStack<Datum>();
		this.env      = env;
		this.error    = false;
		this.accept   = false;
		this.synstack.push(table.getInitialState());
	}

	//
	LispLRParser(LispLRParser p) {
		this.table    = p.table;
		this.action   = p.action;
		this.synstack = new ArrayListStack<Datum>(p.synstack);
		this.semstack = new ArrayListStack<Datum>(p.semstack);
		this.env      = p.env;
		this.error    = p.error;
		this.accept   = p.accept;
	}

	//
	private Datum callaction(LispCFGRule r, LispMessage mesg) {
		List<Datum> l;
		Datum x;
		int n;

		if(action == null)  return LispBoolean.FALSE;
		n = r.getRightValues().size();
		l = new LinkedList<Datum>();
		for(int i = 0; i < n; i++) {
			x = semstack.pop();
			l.add(0, x);
		}
		return Scheme.call(action, env, mesg, LispUtils.listToCons(l));
	}

	/**
	 * 
	 * @param a
	 * @param b
	 * @param mesg
	 * @return
	 */
	public ILispConfiguration goSideEffect(Datum a, Datum b,
			LispMessage mesg) {
		LispCFGRule r;
		Datum s = synstack.peek(), x;

		while(true) {
			if(error) {
				return this;
			} else if(accept) {
				error = true;
				accept = false;
				return this;
			} else if((x = table.getShift(s, a)) != null) {
				synstack.push(x);  semstack.push(b);
				return this;
			} else if((r = table.getReduce(s, a)) != null) {
				synstack.pop(r.getRightValues().size());
				x = synstack.peek();
				if((s = table.getGoto(x, r.getLeftValue())) == null) {
					throw new RuntimeException();
				}
				synstack.push(s);
				semstack.push(callaction(r, mesg));
			} else if(table.isAccept(s, a)) {
				accept = true;
				return this;
			} else {
				error = true;
				return this;
			}
		}
	}

	/**
	 * 
	 * @param d
	 * @param b
	 * @param mesg
	 * @return
	 */
	public ILispConfiguration goSideEffect(int d, Datum b,
			LispMessage mesg) {
		return goSideEffect(LispInteger.valueOf(d), b, mesg);
	}

	/**
	 * 
	 * @param d
	 * @param b
	 * @param mesg
	 * @return
	 */
	public ILispConfiguration goSideEffect(char d, Datum b,
			LispMessage mesg) {
		return goSideEffect(LispCharacter.valueOf(d), b, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(Datum a, LispMessage mesg) {
		return goSideEffect(a, Nil.NIL, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(int d, LispMessage mesg) {
		return goSideEffect(LispInteger.valueOf(d), mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#goSideEffect(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration goSideEffect(char d, LispMessage mesg) {
		return goSideEffect(LispCharacter.valueOf(d), mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#go(net.morilib.lisp.Datum, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration go(Datum a, LispMessage mesg) {
		return new LispLRParser(this).goSideEffect(a, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#go(int, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration go(int d, LispMessage mesg) {
		return new LispLRParser(this).goSideEffect(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#go(char, net.morilib.lisp.LispMessage)
	 */
	@Override
	public ILispConfiguration go(char d, LispMessage mesg) {
		return new LispLRParser(this).goSideEffect(d, mesg);
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		return synstack.size() == 1 && !error && !accept;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#isDead()
	 */
	@Override
	public boolean isDead() {
		return error;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.automata.ILispConfiguration#isAccepted()
	 */
	@Override
	public boolean isAccepted() {
		return accept;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<lr-parser>");
	}

}
