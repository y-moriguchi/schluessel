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
package net.morilib.lisp.math.sequence;

import net.morilib.lisp.LispInteger;
import net.morilib.lisp.LispReal;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2012/02/11
 */
public abstract class AbstractLispRealSequence
extends AbstractLispSequence implements ILispRealSequence {

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#add(net.morilib.lisp.math.sequence.ILispSequence)
	 */
	@Override
	public ILispSequence add(ILispSequence y) {
		final ILispRealSequence t = this;

		if(!(y instanceof ILispRealSequence)) {
			return super.add(y);
		} else {
			final ILispRealSequence s = (ILispRealSequence)y;

			return new AbstractLispRealSequence() {

				public boolean isFinite() {
					return t.isFinite() && s.isFinite();
				}

				public int size() {
					return isFinite() ?
							Math.max(t.size(), s.size()) : -1;
				}

				public LispReal get(int i) {
					return t.get(i).add(s.get(i));
				}

				public LispReal limit() {
					return t.limit().add(s.limit());
				}

				public LispReal sum() {
					return t.sum().add(s.sum());
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#sub(net.morilib.lisp.math.sequence.ILispSequence)
	 */
	@Override
	public ILispSequence sub(ILispSequence y) {
		final ILispRealSequence t = this;

		if(!(y instanceof ILispRealSequence)) {
			return super.add(y);
		} else {
			final ILispRealSequence s = (ILispRealSequence)y;

			return new AbstractLispRealSequence() {

				public boolean isFinite() {
					return t.isFinite() && s.isFinite();
				}

				public int size() {
					return isFinite() ?
							Math.max(t.size(), s.size()) : -1;
				}

				public LispReal get(int i) {
					return t.get(i).subtract(s.get(i));
				}

				public LispReal limit() {
					return t.limit().subtract(s.limit());
				}

				public LispReal sum() {
					return t.sum().subtract(s.sum());
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#uminus()
	 */
	@Override
	public ILispSequence uminus() {
		final ILispRealSequence t = this;

		return new AbstractLispRealSequence() {

			public boolean isFinite() {
				return t.isFinite();
			}

			public int size() {
				return t.size();
			}

			public LispReal get(int i) {
				return t.get(i).negate();
			}

			public LispReal limit() {
				return t.limit().negate();
			}

			public LispReal sum() {
				return t.sum().negate();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#mul(net.morilib.lisp.math.sequence.ILispSequence)
	 */
	@Override
	public ILispSequence mul(ILispSequence y) {
		final ILispRealSequence t = this;

		if(!(y instanceof ILispRealSequence)) {
			return super.add(y);
		} else {
			final ILispRealSequence s = (ILispRealSequence)y;

			return new AbstractLispRealSequence() {

				public boolean isFinite() {
					return t.isFinite() && s.isFinite();
				}

				public int size() {
					return isFinite() ?
							Math.max(t.size(), s.size()) : -1;
				}

				public LispReal get(int i) {
					return t.get(i).multiply(s.get(i));
				}

				public LispReal limit() {
					return t.limit().multiply(s.limit());
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#div(net.morilib.lisp.math.sequence.ILispSequence)
	 */
	@Override
	public ILispSequence div(ILispSequence y) {
		final ILispRealSequence t = this;

		if(!(y instanceof ILispRealSequence)) {
			return super.add(y);
		} else {
			final ILispRealSequence s = (ILispRealSequence)y;

			return new AbstractLispRealSequence() {

				public boolean isFinite() {
					return t.isFinite() && s.isFinite();
				}

				public int size() {
					return isFinite() ?
							Math.max(t.size(), s.size()) : -1;
				}

				public LispReal get(int i) {
					return t.get(i).divide(s.get(i));
				}

				public LispReal limit() {
					return t.limit().divide(s.limit());
				}

			};
		}
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.AbstractLispSequence#inv()
	 */
	@Override
	public ILispSequence inv() {
		final ILispRealSequence t = this;

		return new AbstractLispRealSequence() {

			public boolean isFinite() {
				return t.isFinite();
			}

			public int size() {
				return t.size();
			}

			public LispReal get(int i) {
				return t.get(i).invert();
			}

			public LispReal limit() {
				return t.limit().invert();
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#difference()
	 */
	public ILispRealSequence difference() {
		final ILispRealSequence t = this;

		return new AbstractLispRealSequence() {

			public LispReal get(int i) {
				return t.get(i + 1).subtract(t.get(i));
			}

			public LispReal limit() {
				return t.limit();
			}

			public LispReal sum() {
				return null;
			}

			public boolean isFinite() {
				return t.isFinite();
			}

			public int size() {
				return t.isFinite() ? t.size() - 1 : -1;
			}

		};
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#sum(int, int)
	 */
	public LispReal sum(int start, int end) {
		LispReal r = LispInteger.ZERO;

		if(start < 1)    throw new IllegalArgumentException();
		if(end   < 1)    throw new IllegalArgumentException();
		if(start > end)  throw new IllegalArgumentException();
		for(int i = start; i <= end && i <= size(); i++) {
			r = r.add(get(i));
		}
		return r;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lisp.math.sequence.ILispRealSequence#sum()
	 */
	public LispReal sum() {
		LispReal r = LispInteger.ZERO;

		if(!isFinite()) {
			return null;
		} else {
			for(int i = 1; i <= size(); i++) {
				r = r.add(get(i));
			}
			return r;
		}
	}

}
