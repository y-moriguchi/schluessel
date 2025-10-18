package net.morilib.lang.monad;

/**
 *
 *
 * @author MORIGUCHI, Yuichiro 2010/09/23
 */
public class BooleanMonad {
	
	/**
	 * 
	 *
	 *
	 * @author MORIGUCHI, Yuichiro 2010/10/23
	 */
	public static interface Function {
		
		/**
		 * 
		 * @param b
		 * @return
		 */
		public BooleanMonad f(boolean b);
		
	}
	
	/**
	 * 
	 */
	public static final BooleanMonad TRUE  = new BooleanMonad(true);
	
	/**
	 * 
	 */
	public static final BooleanMonad FALSE = new BooleanMonad(false);
	
	/**
	 * 
	 */
	public static final Function ID = new Function() {

		public BooleanMonad f(boolean x) {
			return getInstance(x);
		}
		
	};
	
	/**
	 * 
	 */
	public static final Function SET = new Function() {

		public BooleanMonad f(boolean x) {
			return TRUE;
		}
		
	};
	
	/**
	 * 
	 */
	public static final Function RESET = new Function() {

		public BooleanMonad f(boolean x) {
			return FALSE;
		}
		
	};
	
	//
	private boolean value;
	
	//
	private BooleanMonad(boolean x) {
		this.value = x;
	}
	
	/**
	 * 
	 * @param x
	 * @return
	 */
	public static BooleanMonad getInstance(boolean x) {
		return x ? TRUE : FALSE;
	}
	
	/**
	 * 
	 * @return
	 */
	public boolean getValue() {
		return value;
	}

	/* (non-Javadoc)
	 * @see net.morilib.lang.monad.Monad#bindOverwrite(java.lang.Object)
	 */
	public BooleanMonad bind(Function klas) {
		return klas.f(value);
	}
	
}