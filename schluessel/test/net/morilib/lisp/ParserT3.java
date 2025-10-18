package net.morilib.lisp;

import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;

public class ParserT3 {
	
	public static void print1(Datum d) {
		if(d instanceof Cons) {
			System.out.print("( ");
			print1(((Cons)d).getCar());
			System.out.print(" . ");
			print1(((Cons)d).getCdr());
			System.out.print(" )");
		} else {
			System.out.print(d);
		}
	}
	
	public static void main(String[] args) throws IOException {
		Reader rd = new InputStreamReader(System.in);
		Scheme lisp = Scheme.newInstance();
		
		lisp.readEvalPrintLoop(rd);
	}
	
}
