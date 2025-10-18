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
package net.morilib.automata.trie;

import java.util.Collections;
import java.util.Map;
import java.util.Set;

import net.morilib.automata.DFAState;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/12/26
 */
public abstract class TrieNode<T, A> implements DFAState<T, A, Void> {

	/**
	 * 
	 */
	protected boolean accepted;

	/**
	 * 
	 * @param accepted
	 */
	protected TrieNode(boolean accepted) {
		this.accepted = accepted;
	}

	/**
	 * 
	 * @param x
	 * @return
	 */
	public abstract TrieNode<T, A> go(T x);

	/**
	 * 
	 * @return
	 */
	public boolean isAccepted() {
		return accepted;
	}

	/* (non-Javadoc)
	 * @see stub.dfa.DFAState#getAccepted()
	 */
	@Override
	public Set<A> getAccepted() {
		if(accepted) {
			return Collections.singleton(getState());
		} else {
			return Collections.emptySet();
		}
	}

	/**
	 * 
	 * @return
	 */
	public TrieNode<T, A> goInt(int x) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public TrieNode<T, A> goChar(char x) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public abstract A getState();

	/**
	 * 
	 * @return
	 */
	public int getStateInt() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public long getStateLong() {
		return getStateInt();
	}

	/**
	 * 
	 * @return
	 */
	public byte getStateByte() {
		return (byte)getStateInt();
	}

	/**
	 * 
	 * @return
	 */
	public short getStateShort() {
		return (short)getStateInt();
	}

	/**
	 * 
	 * @return
	 */
	public char getStateChar() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public boolean getStateBoolean() {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public float getStateFloat() {
		return getStateInt();
	}

	/**
	 * 
	 * @return
	 */
	public double getStateDouble() {
		return getStateDouble();
	}

	/**
	 * 
	 * @return
	 */
	public abstract Map<T, TrieNode<T, A>> getEdges();

	/**
	 * 
	 * @param c
	 * @return
	 */
	public abstract boolean willFail(T t);

	/**
	 * 
	 * @return
	 */
	public boolean willFail(int x) {
		throw new UnsupportedOperationException();
	}

	/**
	 * 
	 * @return
	 */
	public boolean willFail(char x) {
		throw new UnsupportedOperationException();
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#isDead()
	 */
	@Override
	public boolean isDead() {
		return false;
	}

	/* (non-Javadoc)
	 * @see net.morilib.automata.dfa.DFAState#isInitialState()
	 */
	@Override
	public boolean isInitialState() {
		return false;
	}

}
