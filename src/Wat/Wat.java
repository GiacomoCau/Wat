package Wat;

import java.io.File;

public class Wat {
	static Vm vm = new Vm();
	Wat() throws Exception {
		if (!new File("build/boot.wat").exists()) vm.compile("boot.wat");
		vm.exec("boot.wat");
	}
	public static void main(String[] args) throws Exception {
		vm.repl();
	}
}
