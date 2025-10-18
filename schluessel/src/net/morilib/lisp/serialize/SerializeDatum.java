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
package net.morilib.lisp.serialize;

import java.beans.XMLEncoder;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectOutputStream;
import java.io.Serializable;

import net.morilib.lisp.ConsIterator;
import net.morilib.lisp.Datum;
import net.morilib.lisp.Environment;
import net.morilib.lisp.JavaObjective;
import net.morilib.lisp.LispMessage;
import net.morilib.lisp.Subr;
import net.morilib.lisp.Undef;
import net.morilib.lisp.r6rs.io.ILispBinaryOutputPort;
import net.morilib.lisp.subr.SubrUtils;
import net.morilib.util.Objects;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/04/12
 */
public class SerializeDatum extends Subr {

	//
	static void serialize(ObjectOutput oos, Object d,
			LispMessage mesg) throws IOException {
		if(d instanceof Serializable) {
			oos.writeObject(d);
//		} else if(d instanceof Externalizable) {
//			((Externalizable)d).writeExternal(oos);
		} else if(!(d instanceof JavaObjective)) {
			throw mesg.getError(
					"err.serialize.require.serializable",
					Objects.toString(d));
		} else if((d = ((JavaObjective)d).toObject())
				instanceof Serializable) {
			oos.writeObject(d);
//		} else if(d instanceof Externalizable) {
//			((Externalizable)d).writeExternal(oos);
		} else {
			throw mesg.getError(
					"err.serialize.require.serializable",
					Objects.toString(d));
		}
	}

	//
	static void serialize(XMLEncoder oos, Object d,
			LispMessage mesg) throws IOException {
		if(d instanceof Serializable) {
			oos.writeObject(d);
		} else if(!(d instanceof JavaObjective)) {
			throw mesg.getError(
					"err.serialize.require.serializable",
					Objects.toString(d));
		} else if((d = ((JavaObjective)d).toObject())
				instanceof Serializable) {
			oos.writeObject(d);
		} else {
			throw mesg.getError(
					"err.serialize.require.serializable",
					Objects.toString(d));
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.subr.BinaryArgs#execute(net.morilib.lisp.Datum, net.morilib.lisp.Datum, net.morilib.lisp.Environment, net.morilib.lisp.LispMessage)
	 */
	@Override
	public Datum eval(Datum body, Environment env, LispMessage mesg) {
		ConsIterator itr = new ConsIterator(body);
		Datum c1a = SubrUtils.nextIf(itr, mesg, body);
		Datum c2a = SubrUtils.nextIf(itr, mesg, body);
		ObjectOutputStream oos;

		if(c1a instanceof ILispBinaryOutputPort) {
			try {
				oos = new ObjectOutputStream(
						((ILispBinaryOutputPort)c1a).getOutputStream());
				serialize(oos, c2a, mesg);
				return Undef.UNDEF;
			} catch (IOException e) {
				throw mesg.getError("err.io");
			}
		} else {
			throw mesg.getError("err.io.require.port.binary.output",
					c1a);
		}
	}

}
