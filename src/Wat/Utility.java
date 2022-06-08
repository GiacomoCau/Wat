package Wat;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public class Utility {
	
	public static Object[] $(Object... objects) {
		return objects;
	}
	
	public static List toList (Object... objects) {
		return Arrays.stream(objects).toList();
	}
	
	public static boolean isInstance(Object o, Class ... cs) {
		for (Class c: cs) if (c.isInstance(o)) return true;
		return false;
	}
	
	private enum Primitive {
			byte$("B", byte.class, Byte.class),
			short$("S", short.class, Short.class),
			int$("I", int.class, Integer.class),
			long$("J", long.class, Long.class),
			float$("F", float.class, Float.class),
			double$("D", double.class, Double.class),
			boolean$("Z", boolean.class, Boolean.class),
			void$("V", void.class, Void.class)
		;
		public final String type;
		public final Class classe;
		public final Class wrapper;
		public final String name;
		Primitive(String type, Class classe, Class wrapper) {
			this.type = type;
			this.classe = classe;
			this.wrapper = wrapper;
			this.name = classe.getSimpleName();
		}
	}

	public static Class <?> classForName(String name) {
		if (name == null) return null;
		// if (name.equals("")) return null; // va aggiunto?
		// if (name.equals("null")) return null; // va aggiunto?
		boolean L = true;
		for (Primitive p: Primitive.values()) {
			if (!name.startsWith(p.name)) continue;
			if (name.length() == p.name.length()) return p.classe;
			name = p.type + name.substring(p.name.length());
			L = false;
			break;
		}
		L &= name.endsWith("[]");
		if (L) name = "L" + name;
		while (name.endsWith("[]"))	name = "[" + name.substring(0, name.length()-2);
		if (L) name += ";";
		try {
			return Class.forName(name);
		}
		catch (ClassNotFoundException e) {
			return null;
		}
	}
}
