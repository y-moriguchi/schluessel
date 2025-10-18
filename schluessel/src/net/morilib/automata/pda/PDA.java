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
package net.morilib.automata.pda;

import java.util.Map;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2013/01/13
 */
public interface PDA<A, S> {

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean isState(PDAState s);

	/**
	 * 
	 * @param state
	 * @param c
	 * @param s
	 * @return
	 */
	public Map<PDAState, S> transit(PDAState state, A c, S s);

	/**
	 * 
	 * @return
	 */
	public PDAState getInitialState();

	/**
	 * 
	 * @param s
	 * @return
	 */
	public boolean isAccepted(PDAState s);

}
