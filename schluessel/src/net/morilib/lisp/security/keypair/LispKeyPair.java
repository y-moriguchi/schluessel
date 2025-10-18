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
package net.morilib.lisp.security.keypair;

import java.security.KeyPair;

import net.morilib.lisp.Datum2;
import net.morilib.lisp.JavaObjective;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/09
 */
public class LispKeyPair extends Datum2
implements JavaObjective, java.io.Serializable {

	//
	KeyPair keys;

	/**
	 * 
	 * @param keys
	 */
	public LispKeyPair(KeyPair keys) {
		this.keys = keys;
	}

	/**
	 * 
	 * @return
	 */
	public LispPrivateKey getPrivate() {
		return new LispPrivateKey(keys.getPrivate());
	}

	/**
	 * 
	 * @return
	 */
	public LispPublicKey getPublic() {
		return new LispPublicKey(keys.getPublic());
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.JavaObjective#toObject()
	 */
	@Override
	public KeyPair toObject() {
		return keys;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.Datum2#toDisplayString(java.lang.StringBuilder)
	 */
	@Override
	public void toDisplayString(StringBuilder buf) {
		buf.append("#<key-pair>");
	}

}
