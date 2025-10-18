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
package net.morilib.lang.number.complex;

/**
 * 
 *
 *
 * @author MORIGUCHI, Yuichiro 2009
 */
public final class ComplexImaginaryPart implements ImaginaryPart {

	/**
	 * 
	 */
	public static final ComplexImaginaryPart I =
		new ComplexImaginaryPart(true);

	/**
	 * 
	 */
	public static final ComplexImaginaryPart ONE =
		new ComplexImaginaryPart(false);

	//
	private boolean part;

	//
	private ComplexImaginaryPart(boolean part) {
		this.part = part;
	}

	//
	private ImaginaryPart getInstance(boolean part) {
		return part ? I : ONE;
	}

	/*
	 * (non-Javadoc)
	 * @see net.morilib.lang.number.complex.ImaginaryPart#multiply(net.morilib.lang.number.complex.ImaginaryPart)
	 */
	public ImaginaryPart multiply(ImaginaryPart x) {
		ComplexImaginaryPart z = (ComplexImaginaryPart)x;

		return getInstance(part ^ z.part);
	}

	/*
	 * (non-Javadoc)
	 * @see java.lang.Object#toString()
	 */
	public String toString() {
		return part ? "i" : "1";
	}

}
