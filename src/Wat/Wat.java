package Wat;

public class Wat {
	static Vm vm = new Vm();
	Wat() throws Exception {
		vm.compile("boot.wat");
		vm.exec("boot.wat");
	}
	public static void main(String[] args) throws Exception {
		vm.repl();
	}
}
