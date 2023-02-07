package Wat;

import static Wat.Utility.PrimitiveWrapper.toPrimitive;
import static java.lang.System.in;
import static java.lang.System.out;
import static java.lang.reflect.Array.newInstance;
import static java.lang.reflect.Array.set;
import static java.util.Arrays.copyOf;
import static java.util.Arrays.copyOfRange;
import static java.util.Arrays.stream;
import static java.util.Comparator.comparing;
import static java.util.Map.of;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;


public class Utility {
	
	public static Object[] $(Object... objects) {
		return objects;
	}
	/*
	public static <T> T[] $(T... objects) {
		return objects;
	}
	*/
	
	public static <T> boolean equals(T v, T ... a) {
		for (T e: a) if (v.equals(e)) return true;
		return false;
	}
	
	public static <T,R> R apply(Function<T,R> f, T a) {
		return f.apply(a);
	}
	
	public static String eIf(boolean b, String s) {
		return b ? "" : s;
	}
	public static String eIf(boolean b, Supplier<String> s) {
		return b ? "" : s.get();
	}
	public static String eIfnull(String s) {
		return s==null ? "" : s;
	}
	public static String eIfnull(Object o, Supplier<String> s) {
		return o==null ? "" : s.get();
	}
	public static <T> String eIfnull(T o, Function<T, String> f) {
		return o==null ? "" : f.apply(o);
	}
	
	public static <T> T uncked(Callable<T> t) {
		try {
			return t.call();
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
	public static int stackDeep() {
		return new Throwable().getStackTrace().length;
	}
	
	public static List toList(Object... objects) {
		return stream(objects).toList();
	}
	
	public static void main(String[] args) throws Exception {
		out.println(toSource("a\nb"));
		out.println(toString("a\\nb"));
		out.println(toSource(toString("a\\nb")));
		out.println(toString(toSource("a\nb")));
	}
	
	private static Set<Entry<String,String>> control = of("\"", "\\\\\"", "\n", "\\\\n", "\t", "\\\\t", "\r", "\\\\r", "\b", "\\\\b", "\f", "\\\\f").entrySet();
	
	public static String toSource(String s) {
		for (Entry<String,String> e: control) s = s.replaceAll(e.getKey(), e.getValue());
		return s;
	}
	
	public static String toString(String s) {
		for (Entry<String,String> e: control) s = s.replaceAll(e.getValue(), e.getKey());
		return s;
	}
	
	enum Binop {
		Pls((a, b)-> a+b,   (a, b)-> a+b,   (a, b)-> a+b),
		Mns((a, b)-> a-b,   (a, b)-> a-b,   (a, b)-> a-b),
		Pwr((a, b)-> a*b,   (a, b)-> a*b,   (a, b)-> a*b),
		Dvd((a, b)-> a/b,   (a, b)-> a/b,   (a, b)-> a/b),
		Rst((a, b)-> a%b,   (a, b)-> a%b,   (a, b)-> a%b),
		 Ls((a, b)-> a<b,   (a, b)-> a<b,   (a, b)-> a<b),
		 Gt((a, b)-> a>b,   (a, b)-> a>b,   (a, b)-> a>b),
		 Le((a, b)-> a<=b,  (a, b)-> a<=b,  (a, b)-> a<=b),
		 Ge((a, b)-> a>=b,  (a, b)-> a>=b,  (a, b)-> a>=b),
		And((a, b)-> a&b,   (a, b)-> a&b,   null),
		 Or((a, b)-> a|b,   (a, b)-> a|b,   null),
		Xor((a, b)-> a^b,   (a, b)-> a^b,   null),
		 Sl((a, b)-> a<<b,  (a, b)-> a<<b,  null),
		 Sr((a, b)-> a>>b,  (a, b)-> a>>b,  null),
		Sr0((a, b)-> a>>>b, (a, b)-> a>>>b, null);
		
		BiFunction<Integer, Integer, Object> i;
		BiFunction<Double, Double, Object> d;
		BiFunction<Long, Long, Object> l;
		Binop(BiFunction<Integer, Integer, Object> i, BiFunction<Long, Long, Object> l, BiFunction<Double, Double, Object> d) {
			this.i = i; this.l = l; this.d = d;		
		};
	}
	static Object binOp(Binop op, Number a, Number b) {
		if (op == null) throw new RuntimeException("no operator for this operands");
		return a instanceof Integer i1 && b instanceof Integer i2
			? op.i.apply(i1, i2)
			: op.d.apply(a.doubleValue(), b.doubleValue())
		;
		/*
		return (a, b)->
			a instanceof Double d ? op.d.apply(d, b.doubleValue())
			: b instanceof Double d ? op.d.apply(a.doubleValue(), d)
			: op.i.apply(a.intValue(), b.intValue())
		;
		*/
	}	
	
	public static boolean isInstance(Object o, Class ... cs) {
		for (Class c: cs) if (c.isInstance(o)) return true;
		return false;
	}
	
	enum PrimitiveWrapper {
			byte$('B', byte.class, Byte.class),
			short$('S', short.class, Short.class),
			char$('C', char.class, Character.class),
			int$('I', int.class, Integer.class),
			long$('J', long.class, Long.class),
			float$('F', float.class, Float.class),
			double$('D', double.class, Double.class),
			boolean$('Z', boolean.class, Boolean.class),
			void$('V', void.class, Void.class)
		;
		public final char type;
		public final Class primitive;
		public final Class wrapper;
		public final String name;
		PrimitiveWrapper(char type, Class primitive, Class wrapper) {
			this.type = type;
			this.primitive = primitive;
			this.wrapper = wrapper;
			this.name = primitive.getSimpleName();
		}
		public static Class toWrapper(Class type) {
			for (var p: values()) if (type == p.primitive) return p.wrapper;
			return type;
		}
		public static Class toPrimitive(Class type) {
			for (var p: values()) if (type == p.wrapper) return p.primitive;
			return type;
		}
	}
	
	public static Class <?> classForName(String name) {
		if (name == null) return null;
		boolean L = true;
		for (var p: PrimitiveWrapper.values()) {
			if (!name.startsWith(p.name)) continue;
			if (name.length() == p.name.length()) return p.primitive;
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
			//return null;
			throw new Error("not a class: &" + name, e);
		}
	}
	
	public static String toSource(Class cl) {
		var str = cl.getName();
		var dim = ""; int i=0; for (; str.charAt(i) == '['; i+=1) dim += "[]";
		if (dim.length() == 0) return str;
		//str = str.substring(i);
		char c = str.charAt(i);
		if (c == 'L') return str.substring(i+1, str.length()-1) + dim;
		for (var p: PrimitiveWrapper.values()) if (p.type == c) return p.name + dim;
		return cl.toString();
	}
	
	/* TODO sostituito dal seguente eliminare appena verificato
	public static Object[] reorg(Class[] classi, Object[] args) throws Exception {
		int length = classi.length;
		/* TODO verificare l'ottimizzazione, eliminare altrimenti
		if (length == 1 && !classi[0].isPrimitive()) return args;
	 	* /
		Object[] newArgs = Arrays.copyOf(args, length); // eventualmente null in più!
		if (args.length < length) return newArgs;
		Class componentType = classi[length-=1].getComponentType();
		if (!componentType.isPrimitive()) {
			//newArgs[length] = Arrays.copyOfRange(args, length, args.length, (Class) Array.newInstance(componentType, 0).getClass()); // ok
			newArgs[length] = Arrays.copyOfRange(args, length, args.length, getClassArray(componentType)); // ok
		}
		else {
			Object last = Array.newInstance(componentType, args.length-length);
			for (int i=length; i<args.length; i+=1) Array.set(last, i-length, args[i]);
			newArgs[length] = last;
		}
		return newArgs;
	}
	*/
	public static Object[] reorg(Executable ex, Object[] args) throws Exception {
		return reorg(ex.isVarArgs(), ex.getParameterTypes(), args); 
	}
	/* TODO sostituito dal seguente eliminare appena verificato
	public static Object[] reorg(boolean isVarArgs, Class[] parms, Object[] args) throws Exception {
		int length = parms.length;
		if (!isVarArgs) {
			if (length != 1 || !parms[0].isArray()) return args;
			return new Object[] { copyOf(args, length, getClassArray(parms[0].componentType())) };
		}
		Object[] newArgs = copyOf(args, length); // eventualmente null in più!
		if (args.length < length) return newArgs;
		Class componentType = parms[length-=1].getComponentType();
		if (!componentType.isPrimitive()) {
			newArgs[length] = copyOfRange(args, length, args.length, getClassArray(componentType));
		}
		else {
			Object last = Array.newInstance(componentType, args.length-length);
			for (int i=length; i<args.length; i+=1) set(last, i-length, args[i]);
			newArgs[length] = last;
		}
		return newArgs;
	}
	private static Object[] reorg(boolean isVarArgs, Class[] parms, Object[] args) throws Exception {
		int length = parms.length;
		if (!isVarArgs) {
			if (length != 1 || !parms[0].isArray()) return args;
			var cType = parms[0].componentType();
			if (args.getClass().componentType() == cType) return new Object[] { args };
			if (!cType.isPrimitive()) {
				return new Object[] { copyOfRange(args, 0, args.length, getClassArray(cType)) };
			}
			Object newArgs = newInstance(cType, length);
			for (int i=0; i<args.length; i+=1) Array.set(newArgs, i-length, args[i]);
			return new Object[] { newArgs };
		}
		Object[] newArgs = copyOf(args, length); // eventualmente null in più!
		if (args.length < length) return newArgs;
		Class cType = parms[length-=1].getComponentType();
		if (!cType.isPrimitive()) {
			newArgs[length] = copyOfRange(args, length, args.length, getClassArray(cType));
		}
		else {
			Object lastArg = newInstance(cType, args.length-length);
			for (int i=length; i<args.length; i+=1) Array.set(lastArg, i-length, args[i]);
			newArgs[length] = lastArg;
		}
		return newArgs;
	}
	*/
	public static Object[] reorg(boolean isVarArgs, Class[] parms, Object[] args) throws Exception {
		int length = parms.length;
		if (!isVarArgs) {
			if (length != 1 || !parms[0].isArray()) return args;
			var cType = parms[0].componentType();
			return new Object[] { args.getClass().componentType() == cType ?  args : copyFrom(cType, 0, args) };
		}
		Object[] newArgs = copyOf(args, length); // eventualmente un null in più!
		if (args.length < length) return newArgs;
		var cType = parms[length-=1].getComponentType();
		newArgs[length] = copyFrom(cType, length, args);
		return newArgs;
	}
	private static Object copyFrom(Class cType, int from, Object[] args) {
		if (!cType.isPrimitive()) return copyOfRange(args, from, args.length, getClassArray(cType));
		var newArgs = newInstance(cType, args.length-from);
		for (int i=from; i<args.length; i+=1) set(newArgs, i-from, args[i]);
		return newArgs;
	}
	
	public static Class getClassArray(Class c) {
		return Array.newInstance(c, 0).getClass();
		/* TODO in alternativa al precedente, eliminare altrimenti
		return getClassArray(c, 1);
		*/
	}
	
	public static Class getClassArray(Class c, int n) {
		return Array.newInstance(c, new int[n]).getClass();
	}
	
	static boolean publicMember = false;
	
	public static Field getField(Class <?> classe, String name) {
		return getField(classe, publicMember, name);
	}
	public static Field getField(Class <?> classe, boolean onlyPublicMember, String name) {
		do {
			try {
				return onlyPublicMember ? classe.getField(name) : classe.getDeclaredField(name);
			}
			catch (Exception e) {
				if (onlyPublicMember) return null;
			}
		} while ((classe = classe.getSuperclass()) != null);
		return null;
	}
	
	public static void getConstructor() throws IllegalAccessException, InvocationTargetException {
		//Class c = Integer.class;
		//out.println(getField(Integer.class, "MAX_VALUE"));
		//out.println();
		
		//out.println(getExecutable((Object) Integer.class, "equals", new Object[] { Object.class }));
		//out.println();
		//out.println(getExecutable((Object) Prova.Box.class, "getMethod", new Object[] { "get" }));
		//out.println();
		//out.println(getExecutable((Object) Prova.Box.class, "getMethod", new Object[] { "set", 1 }));
		//out.println();
		
		out.println(getExecutable(Prova.Box.class, "getMethod", new Class[] { String.class, Class[].class } ));
		out.println();
		out.println(getExecutable(Prova.Box.class, "getConstructor", new Class[] { Prova.Box.class, Class[].class } ));
		out.println();
		
		Method m = (Method) getExecutable(Prova.Box.class, "getConstructor", Class[].class );
		out.println(m);
		out.println();
		
		out.println(m.invoke((Object) Prova.Box.class, int.class));
		//Constructor c2 = (Constructor) m.invoke((Object) Class.class , int.class);
		
		//out.println(getExecutable((Object) Class.class, "getMethod", new Object[] { String.class, Class[].class } ));
		//out.println(getExecutable((Object) Integer.class, "invoke", new Object[] { String.class }));
		//out.println(getExecutable((Object) Integer.class, "new", new Object[] { String.class }));
		//out.println(getExecutable((Object) Integer.class, "newInstance", new Object[] { String.class }));
	}
	
	public static Executable getExecutable(Object obj, String name, Object ... args) {
		return getExecutable(obj.getClass(), name, getClasses(args));
	}
	
	public static Class[] getClasses(Object ... objects) {
		return stream(objects).map(o-> o == null ? null : o instanceof Class c ? c : o.getClass()).toArray(Class[]::new);
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, String name) {
		return getExecutable(classe, name, (Class[]) null);
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, String name, Class <?> ... classes) {
		return getExecutable(classe, publicMember, name, classes);
		//T executable = getExecutable(classe, publicMember, name, classes);
		//return executable != null ? executable : getExecutable(classe.getClass(), publicMember, name, classes);
	}
	
	private static boolean trace = false; 
	
	private static String toString(String name, Class <?> ... argumentsClass) {
		String s = stream(argumentsClass).map(c-> c == null ? null : c.toString()).toList().toString();
		return name + "(" + s.substring(1, s.length()-1) + ")";
	}
	
	public static <T extends Executable> T getExecutable(Class <?> classe, boolean publicMember, String name, Class <?> ... argumentsClass) {
		if (trace) out.println(classe + " " + toString(name, argumentsClass));
		boolean constructors = name.equals("new");
		List<Executable> executables = new ArrayList<>();
		do {
			if (trace) out.println("\tc: " + classe);
			try {
				var t = (T) getExecutor(classe, constructors, publicMember, name, argumentsClass);
				if (trace) out.println("\t\t1: " + t);
				return t;
			}
			catch (NoSuchMethodException nsme) {
				for (Executable executor: getExecutors(classe, constructors, publicMember)) {
					if (!constructors && !executor.getName().equals(name)) continue;
					if (trace) out.println("\t\tn: " + executor);
					if (argumentsClass == null) return (T) executor; // si va per nome, non si possono controllare gli argomenti
					boolean isVarArgs = executor.isVarArgs();
					Class [] parametersClass = executor.getParameterTypes();
					if (!isVarArgs && argumentsClass.length != parametersClass.length || argumentsClass.length < parametersClass.length - 1) continue;
					if (!isInvokeConvertible(isVarArgs, parametersClass, argumentsClass)) continue;
					executables.add(executor);
				}
				if (publicMember) break;
			}
		} while ((classe = classe.getSuperclass()) != null);
		
		if (trace) out.println("f: " + executables.size());
		switch (executables.size()) {
			case 0: throw new RuntimeException(toString(name, argumentsClass) + " non trovato in " + classe);
			case 1: return (T) executables.get(0);
			default: {
				executables.sort(comparing(Executable::toString));
				Executable bestMatch = null;
				Float bestCost = Float.MAX_VALUE;
				for (var executable: executables) {
					var cost = getTotalTransformationCost(executable, argumentsClass);
					if (cost >= bestCost) continue;
					if (cost == bestCost) throw new RuntimeException("I " + (name.equals("new") ? "costruttori" : "metodi") + " " + bestMatch + " and " + executable + " has equal cost: " + bestCost);
					bestCost = cost;
					bestMatch = executable;
				}
				if (bestMatch != null) return (T) bestMatch;
				throw new RuntimeException((name.equals("new") ? "Costruttore" : "Metodo") + " non univoco." + executables);
			}
		}
	}
	
	public static float getTotalTransformationCost(final Executable executable, final Class<?>[] srcArgs) {
		final Class<?>[] destArgs = executable.getParameterTypes();
		final boolean isVarArgs = executable.isVarArgs();
		
		// "source" and "destination" are the actual and declared args respectively.
		float totalCost = 0.0f;
		final long normalArgsLen = isVarArgs ? destArgs.length-1 : destArgs.length;
		if (srcArgs.length < normalArgsLen) return Float.MAX_VALUE;
		
		for (int i = 0; i < normalArgsLen; i++) totalCost += getObjectTransformationCost(srcArgs[i], destArgs[i]);
		
		if (isVarArgs) {
			// When isVarArgs is true, srcArgs and dstArgs may differ in length.
			// There are two special cases to consider:
			final boolean noVarArgsPassed = srcArgs.length < destArgs.length;
			final boolean explicitArrayForVarargs =
				srcArgs.length == destArgs.length
				&& srcArgs[srcArgs.length-1] != null
				&& srcArgs[srcArgs.length-1].isArray()
			;
			
			final float varArgsCost = 0.001f;
			final Class<?> destClass = destArgs[destArgs.length-1].getComponentType();
			if (noVarArgsPassed) {
				// When no varargs passed, the best match is the most generic matching type, not the most specific.
				totalCost += varArgsCost + getObjectTransformationCost(destClass, Object.class);
			}
			else if (explicitArrayForVarargs) {
				final Class<?> srcClass = srcArgs[srcArgs.length-1].getComponentType();
				totalCost += varArgsCost + getObjectTransformationCost(srcClass, destClass);
			}
			else {
				// This is typical varargs case.
				for (int i = destArgs.length-1; i < srcArgs.length; i++) {
					final Class<?> srcClass = srcArgs[i];
					totalCost += varArgsCost + getObjectTransformationCost(srcClass, destClass);
				}
			}
		}
		return totalCost;
	}
	
	private static float getObjectTransformationCost(Class<?> srcClass, final Class<?> destClass) {
		if (destClass.isPrimitive()) return getPrimitivePromotionCost(srcClass, destClass);
		
		float cost = 0.0f;
		while (srcClass != null && !destClass.equals(srcClass)) {
			//if (destClass.isInterface() && ClassUtils.isAssignable(srcClass, destClass)) {
			if (destClass.isInterface() && isAssignableFrom(destClass, srcClass)) {
				// slight penalty for interface match.
				// we still want an exact match to override an interface match,
				// but
				// an interface match should override anything where we have to
				// get a superclass.
				cost += 0.25f;
				break;
			}
			cost++;
			srcClass = srcClass.getSuperclass();
		}
		// If the destination class is null, we've traveled all the way up to
		// an Object match. We'll penalize this by adding 1.5 to the cost.
		if (srcClass == null) cost += 1.5f;
		return cost;
	}
	
	private static float getPrimitivePromotionCost(final Class<?> srcClass, final Class<?> destClass) {
		if (srcClass == null) return 1.5f;
		
		float cost = 0.0f;
		Class<?> cls = srcClass;
		if (!cls.isPrimitive()) {
			// slight unwrapping penalty
			cost += 0.1f;
			//cls = ClassUtils.wrapperToPrimitive(cls);
			cls = toPrimitive(cls);
		}
		Class<?>[] primitiveTypes = { Byte.TYPE, Short.TYPE, Character.TYPE, Integer.TYPE, Long.TYPE, Float.TYPE, Double.TYPE };
		for (int i = 0; cls != destClass && i < primitiveTypes.length; i++) {
			if (cls != primitiveTypes[i]) continue;
			cost += 0.1f;
			if (i >= primitiveTypes.length - 1) continue;
			cls = primitiveTypes[i + 1];
		}
		return cost;
	}
	
	private static Executable getExecutor(Class<?> classe, boolean constructors, boolean publicMember, String name, Class<?>... classes)
	throws NoSuchMethodException {
		return constructors
			? (publicMember ? classe.getConstructor(classes) : classe.getDeclaredConstructor(classes))
			: (publicMember ? classe.getMethod(name, classes) : classe.getDeclaredMethod(name, classes))
		;
	}
	
	private static Executable[] getExecutors(Class<?> classe, boolean constructors, boolean publicMember) {
		return constructors
			? (publicMember ? classe.getConstructors() : classe.getDeclaredConstructors())
			: (publicMember ? classe.getMethods() : classe.getDeclaredMethods())
		;
	}
	
	private static boolean isInvokeConvertible(boolean varArg, Class[] parameters, Class[] arguments) {
		if (!varArg) return isInvokeConvertible(parameters, arguments);
		int i=0;
		for (int e=parameters.length-1; i<e; i+=1) {
			if (!isInvokeConvertible(parameters[i], arguments[i])) return false;
		}
		if (parameters.length == arguments.length && isAssignableFrom(parameters[i], arguments[i])) return true;
		Class componentType = parameters[i].getComponentType();
		for (int e=arguments.length; i<e; i+=1) {
			if (!isInvokeConvertible(componentType, arguments[i])) return false;
		}
		return true;
	}
	private static boolean isInvokeConvertible(Class[] ps, Class[] as) {
		for (int i=0; i<as.length; i+=1) {
			if (!isInvokeConvertible(ps[i], as[i])) return false;
		}
		return true;
	}
	
	private static boolean isInvokeConvertible(Class p, Class a) {
		return isAssignableFrom(p, a) || isWPConvertible(p, a);
	}
	
	// Identity and Wideninig Reference Conversion
	private static boolean isAssignableFrom(Class t, Class f) {
		/* TODO potrebbe essere utile, eliminare altrimenti
		if (t == null) return false;
		*/
		if (f == null) return !t.isPrimitive();
		/* TODO non necessario, eliminare
		if (f.isPrimitive()) f = wrapper(f);
		*/
		if (t == Class.class && t ==  f.getClass()) return true;
		return t.isAssignableFrom( f );
	}
	
	/*
	private static boolean isAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return true;
		/* TODO in alternativa al precedente; eliminare altrimenti
		for (Class f: fs) if (f == null ? !t.isPrimitive() : t.isAssignableFrom(f)) return true;
		* /
		return false;
	}
	
	private static Class getAssignableFrom(Class t, Class ... fs) {
		for (Class f: fs) if (isAssignableFrom(t, f)) return f;
		return null;
	}
	*/
	
	// Identity and Widening Primitive Conversion
	private static boolean isWPConvertible(Class t, Class f) {
		/* TODO verificare l'ottimizzazione, eliminare altrimenti
		if (!t.isPrimitive() || f == null || f.isPrimitive()) return false;
		//*/
		switch (f == null ? "" : f.getSimpleName()) {
			case "Boolean":   return t == boolean.class;
			case "Byte":      if (t == byte.class  ) return true;
			case "Short":     if (t == short.class ) return true;
							  if (t == char.class  ) return false; // Byte e Short non vanno a char
			case "Character": if (t == char.class  ) return true;
			case "Integer":   if (t == int.class   ) return true;
			case "Long":      if (t == long.class  ) return true;
			case "Float":     if (t == float.class ) return true;
			case "Double":    if (t == double.class) return true;
			default:          return false;
		}
	}
	
	static public String read() throws IOException {
		return read(in);
	}
	static public String read(InputStream in) throws IOException {
		var s = new StringBuilder();
		int open = 0, close = 0;
		boolean inEscape = false, inString = false, inUSymbol = false, inComment=false, sMlComment=false, inMlComment=false, eMlComment=false;
		do {
			//out.println("loop");
			var oc = close-open;
			if (in.available() == 0) out.print(oc==0 ? "> " : "%+d%s> ".formatted(oc, oc>0 ? "(" : ")"));
			for (int c; (c = in.read()) != '\n' || inString || inUSymbol || inMlComment;) {
				//out.printf("%d %c \n",c, c==-1?' ':c);
				if (c == -1) return "";
				if (inEscape) {
					inEscape = false;
				}
				else if (inString) switch (c) {
					case '\\'-> inEscape = true;
					case '"'-> inString = false;
				}
				else if (inUSymbol) switch (c) {
					case '|'-> inUSymbol = false;
				}
				else if (inComment) switch (c) {
					case '"'-> inString = true;
					case '\n'-> inComment = false;
				}
				else if (eMlComment) switch (c) {
					case '#'-> inMlComment = eMlComment = false;
					default -> inUSymbol = !(eMlComment = false);
				}
				else if (inMlComment) switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '|'-> eMlComment = true;
				}
				else if (sMlComment) switch (c) {
					case '|'-> inMlComment = !(sMlComment = false);
					default -> sMlComment = false;
				}
				else switch (c) {
					case '"'-> inString = true;
					case '|'-> inUSymbol = true;
					case ';'-> inComment = true;
					case '#'-> sMlComment = true;
					case '('-> open += 1;
					case ')'-> close += 1;
				}
				if (c >= 32 || inUSymbol) s.append((char) c);
			}
			if (inComment) { inComment = false; }
			s.append('\n'); 
		} while (open > close);
		return s.toString();
	}

}
