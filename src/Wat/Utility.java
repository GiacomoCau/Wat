package Wat;

import static Wat.Utility.PrimitiveWrapper.toPrimitive;
import static java.lang.Character.toUpperCase;
import static java.lang.Integer.parseInt;
import static java.lang.Integer.toHexString;
import static java.lang.System.arraycopy;
import static java.lang.System.in;
import static java.lang.System.out;
import static java.lang.reflect.Array.newInstance;
import static java.lang.reflect.Array.set;
import static java.nio.charset.Charset.forName;
import static java.util.Arrays.copyOf;
import static java.util.Arrays.copyOfRange;
import static java.util.Arrays.stream;
import static java.util.Comparator.comparing;
import static java.util.Map.of;
import static java.util.regex.Pattern.compile;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.lang.reflect.Array;
import java.lang.reflect.Executable;
import java.lang.reflect.Field;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Pattern;

import List.ParseException;


public class Utility {
	
	//*
	public static void main(String[] args) throws Exception {
		//out.println(toSource("a\nb"));
		//out.println(toString("a\\nb"));
		//out.println(toSource(toString("a\\nb")));
		//out.println(toString(toSource("a\nb")));
		//out.println(Charset.defaultCharset());
		//out.println(getProperty("stdout.encoding"));
		//var isr = new InputStreamReader(in, forName("UTF-8"));
		//for(;;) { var v = read(/*0, isr*/); out.print(v); }
		//out.println(new File("reference/*.html").list().length);
		//Files.exists(new Path(" "));
		stdCtrl();
	}
	//*/
	
	public static void stdCtrl() {
		var str = "aa\\n\\\\rx\\rc\\c\\td\\\"f";
		out.println(str);
		
		// toString
		var pat1 = compile("\\\\[\"nrtbf\\\\]");
		var map1 = of("\\\"", "\"", "\\n","\n", "\\r","\r", "\\t","\t", "\\b","\b", "\\f","\f", "\\\\", "\\\\\\\\");
		var res = pat1.matcher(str).replaceAll(mr-> map1.get(mr.group()));
		out.println(res);
		
		//toSource
		var pat2 = compile("[\"\n\r\t\b\f\\\\]");
		var map2 = of("\"","\"", "\n","n", "\r","r", "\t","t", "\b","b", "\f","f", "\\","");
		var org = pat2.matcher(res).replaceAll(mr-> "\\\\" + map2.get(mr.group()));
		out.println(org);
	}
	
	public static int more = Integer.MAX_VALUE;
	public static Object string = new Object();
	
	public static Object[] $(Object... objs) {
		return objs;
	}
	public static <T> T $n(int i, Object ... objs) {
		return (T) ((Object[]) objs)[i];
	}
	public static Object or(Object... objs) {
		return objs;
	}
	public static Object first(Object ... objs) {
		return objs[0];
	}
	public static Object last(Object ... objs) {
		return objs[objs.length-1];
	}
	
	public static <T> boolean member(T v, T ... a) {
		for (T e: a) if (v == null ? e == null : v.equals(e)) return true;
		return false;
	}
	
	public static <T,R> R apply(Function<T,R> f, T a) {
		return f.apply(a);
	}
	public static <T,U,R> R apply(BiFunction<T,U,R> f, T a, U b) {
		return f.apply(a, b);
	}
	
	public static <T> T[] tailAdd(T[] first, T ... second) {
		T[] res = copyOf(first, first.length + second.length);
		arraycopy(second, 0, res, first.length, second.length);
		return res;
	}
	public static <T> T[] headAdd(T[] second, T ... first) {
		T[] res = copyOf(first, second.length + first.length);
		arraycopy(second, 0, res, first.length, second.length);
		return res;
	}
	
	public static String capitalize(String s) {
		return toUpperCase(s.charAt(0)) + s.substring(1);
	}
	public static String camelize(String ss, String dot) {
		String a[] = ss.split(dot), r = a[0];
		for (int i=1; i<a.length; i+=1) r += capitalize(a[i]);
		return r;
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
	
	public static <T> T ifnull(T t, T s) {
		return t != null ? t : s;
	}
	public static <T> T ifnull(T t, Supplier<T> s) {
		return t != null ? t : s.get();
	}
	public static <T,R> R ifnull(T t, R r, Function<T, R> f) {
		return t != null ? f.apply(t) : r;
	}
	
	public static <T> T uncked(Callable<T> t) {
		try {
			return t.call();
		}
		catch (Vm.Error|Vm.Value err) {
			throw err;
		}
		catch (ParseException pe) {
			throw new RuntimeException(getMessage(pe));
		}
		catch (Exception e) {
			throw new RuntimeException(e);
		}
	}
	
    public static int system(boolean b, String ... args) throws Exception {
		ProcessBuilder builder = new ProcessBuilder(args);
		builder.redirectErrorStream(true);
		Process process = builder.start();
		BufferedReader r = new BufferedReader(new InputStreamReader(process.getInputStream()));
		for (String line=null; (line = r.readLine()) != null; ) if (b) System.out.println(line);
		return process.exitValue();
    }
    
	public static String getMessage(ParseException pe) {
		return "ParseException " + pe.getMessage().replaceAll("\r\n", " ").replaceAll("\\.\\.\\.", "").replaceAll(" +", " ");
	}	
	
	public static int stackDeep() {
		return new Throwable().getStackTrace().length;
	}
	
	public static List toList(Object... objects) {
		return stream(objects).toList();
	}
	
	private static Pattern pat1 = compile("\\\\[\"nrtbf\\\\]");
	private static Map<String,String> map1 = of("\\\"", "\"", "\\n","\n", "\\r","\r", "\\t","\t", "\\b","\b", "\\f","\f", "\\\\", "\\\\\\\\");
	private static Pattern xEscape = Pattern.compile("\\\\x([0-9a-bA-b]{1,4});");
	
	public static String toString(String s) {
		s = pat1.matcher(s).replaceAll(mr-> map1.get(mr.group()));
		var m = xEscape.matcher(s);
		if (!m.find()) return s;
		var sb = new StringBuffer();
		do m.appendReplacement(sb, "" + (char) parseInt(m.group(1), 16)); while (m.find());
		return m.appendTail(sb).toString();
	}
	
	private static Pattern pat2 = compile("[\"\n\r\t\b\f\\\\]");
	private static Map<String,String> map2 = of("\"","\"", "\n","n", "\r","r", "\t","t", "\b","b", "\f","f", "\\","");
	private static Pattern isoControl = Pattern.compile("[\\x00-\\x1f\\x7f-\\x9f]");
	
	public static String toSource(String s) {
		s = pat2.matcher(s).replaceAll(mr-> "\\\\" + map2.get(mr.group()));
		var m = isoControl.matcher(s);
		if (!m.find()) return s;
		var sb = new StringBuffer();
		do m.appendReplacement(sb, "\\\\x" + toHexString(m.group().charAt(0)) + ";"); while (m.find()); 
		return m.appendTail(sb).toString();
	}
	
	public static String encode(String a) {
		if (a == null) return "";
		if (a.trim().length() == 0) return "&nbsp;";
		char c;
		StringBuffer sb = new StringBuffer(2*a.length());
		int i = 0, end = a.length();
		for (; i < end && (c = a.charAt(i)) == ' '; i++) sb.append("&nbsp;");
		for (; i < end; i++) {
			switch (c = a.charAt(i)) {
				case '<': sb.append("&lt;"); break;
				case '>': sb.append("&gt;"); break;
				case '&': sb.append("&amp;"); break;
				case '"': sb.append("&quot;"); break;
				case '\n': sb.append("<br>"); break;
				case '\t': sb.append("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"); break;
				default: sb.append(c); break;
			}
		}
		return sb.toString();
	}

	enum BinOp {
		//  Integer         Long            BigInteger                           Double                   BigDecimal 
		Pls((a, b)-> a+b,   (a, b)-> a+b,   (a, b)-> a.add(b),                   (a, b)-> a+b,            (a, b)-> a.add(b)            ),
		Mns((a, b)-> a-b,   (a, b)-> a-b,   (a, b)-> a.subtract(b),              (a, b)-> a-b,            (a, b)-> a.subtract(b)       ),
		Pwr((a, b)-> a*b,   (a, b)-> a*b,   (a, b)-> a.multiply(b),              (a, b)-> a*b,            (a, b)-> a.multiply(b)       ),
		Dvd((a, b)-> a/b,   (a, b)-> a/b,   (a, b)-> a.divide(b),                (a, b)-> a/b,            (a, b)-> a.divide(b)         ),
		Rst((a, b)-> a%b,   (a, b)-> a%b,   (a, b)-> a.mod(b),                   (a, b)-> a%b,            null                         ),
		And((a, b)-> a&b,   (a, b)-> a&b,   (a, b)-> a.and(b),                   null,                    null                         ),
		 Or((a, b)-> a|b,   (a, b)-> a|b,   (a, b)-> a.or(b),                    null,                    null                         ),
		Xor((a, b)-> a^b,   (a, b)-> a^b,   (a, b)-> a.pow(b.intValue()),        (a, b)-> Math.pow(a, b), (a, b)-> a.pow(b.intValue()) ),
		 Sl((a, b)-> a<<b,  (a, b)-> a<<b,  (a, b)-> a.shiftLeft(b.intValue()),  null,                    null                         ),
		 Sr((a, b)-> a>>b,  (a, b)-> a>>b,  (a, b)-> a.shiftRight(b.intValue()), null,                    null                         ),
		Sr0((a, b)-> a>>>b, (a, b)-> a>>>b, null,                                null,                    null                         );                  
		
		BiFunction<BigDecimal, BigDecimal, Object> bd;
		BiFunction<BigInteger, BigInteger, Object> bi;
		BiFunction<Integer, Integer, Object> i;
		BiFunction<Double, Double, Object> d;
		BiFunction<Long, Long, Object> l;
		BinOp(
			BiFunction<Integer, Integer, Object> i,
			BiFunction<Long, Long, Object> l,
			BiFunction<BigInteger, BigInteger, Object> bi,
			BiFunction<Double, Double, Object> d,
			BiFunction<BigDecimal, BigDecimal, Object> bd
		) {
			this.i = i; this.l = l; this.bi=bi; this.d = d; this.bd = bd;		
		};
	}
	static Object binOp(BinOp op, Number a, Number b) {
		if (op == null) throw new RuntimeException("no operator for this operands");
		return a instanceof BigDecimal || b instanceof BigDecimal
			? op.bd.apply(new BigDecimal(""+a), new BigDecimal(""+b))
			: a instanceof Double || b instanceof Double
			? op.d.apply(a.doubleValue(), b.doubleValue())
			: a instanceof BigInteger || b instanceof BigInteger
			? op.bi.apply(new BigInteger(""+a), new BigInteger(""+b))
			: a instanceof Long || b instanceof Long
			? op.l.apply(a.longValue(), b.longValue())
			: op.i.apply(a.intValue(), b.intValue())
		;
	}	
	
	public static boolean isInstance(Object o, Class ... cs) {
		for (Class c: cs) if (c.isInstance(o)) return true;
		return false;
	}
	public static boolean isSubclass(Class c0, Class ... cs) {
		for (Class c: cs) if (c.isAssignableFrom(c0)) return true;
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
			throw new RuntimeException("not a java class: &" + name, e);
		}
	}
	
	public static String toSource(Class cl) {
		var str = cl.getName();
		//var str = cl.getCanonicalName();
		var dim = ""; int i=0; for (; str.charAt(i) == '['; i+=1) dim += "[]";
		if (dim.length() == 0) return str;
		//str = str.substring(i);
		char c = str.charAt(i);
		if (c == 'L') return str.substring(i+1, str.length()-1) + dim;
		for (var p: PrimitiveWrapper.values()) if (p.type == c) return p.name + dim;
		return "&" + cl.toString();
	}
	
	public static Object[] reorg(Executable ex, Object[] args) {
		return reorg(ex.isVarArgs(), ex.getParameterTypes(), args); 
	}
	public static Object[] reorg(boolean isVarArgs, Class[] parms, Object[] args) {
		int length = parms.length;
		if (!isVarArgs) {
			if (length != 1 || !parms[0].isArray()) return args;
			var cType = parms[0].componentType();
			return new Object[] { args.getClass().componentType() == cType ?  args : copyFrom(cType, 0, args) };
		}
		if (args.length == length && (args[length-1] == null || args[length-1].getClass() == parms[length-1])) return args;
		Object[] newArgs = copyOf(args, length); // eventualmente un null in più!
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
	private static boolean trace = false; 
	
	public static Class[] getClasses(Object ... objects) {
		return stream(objects).map(o-> o == null ? null : o instanceof Class c ? c : o.getClass()).toArray(Class[]::new);
	}
	
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
	
	public static <T extends Executable> T getExecutable(Object obj, String name, Class <?> ... classes) {
		return getExecutable(obj, publicMember, name, classes);
	}
	public static <T extends Executable> T getExecutable(Object obj, boolean publicMember, String name, Class <?> ... argumentsClass) {
		boolean constructors = name.equals("new");
		Class <?> classe = constructors ? (Class) obj : obj.getClass();
		if (trace) out.println(classe + " " + toString(name, argumentsClass));
		// per i costruttori obj è una classe e solo su quella vanno cercati 
		// i metodi invece vanno cercati sulla classe di obj e le relative super classi
		// se obj è una classe vanno anche cercati su obj stesso e le relative super classi
		// escludendo Class e Object perché già controllate con la prima ricerca 
		List<Executable> executables = new ArrayList<>();
		do addExecutable(executables, classe, publicMember, constructors, name, argumentsClass);
		while (!constructors && !publicMember && (classe = classe.getSuperclass()) != null);
		if (!constructors && obj instanceof Class && !member(obj, Class.class, Object.class)) {
			classe = (Class) obj;
			do addExecutable(executables, classe, publicMember, constructors, name, argumentsClass);
			while (!publicMember && (classe = classe.getSuperclass()) != Object.class && classe != null);
		}
		
		if (trace) out.println("found: " + executables.size());
		switch (executables.size()) {
			case 0: return null; //throw new RuntimeException(toString(name, argumentsClass) + " non trovato in " + classe);
			case 1: return (T) executables.get(0);
			default: {
				executables.sort(comparing(Executable::toString));
				Executable bestMatch = null;
				Float bestCost = Float.MAX_VALUE;
				for (var executable: executables) {
					if (bestMatch == null) {
						bestMatch = executable;
						bestCost = getTotalTransformationCost(executable, argumentsClass);
						continue;
					} 
					var cost = getTotalTransformationCost(executable, argumentsClass);
					if (cost > bestCost) continue;
					if (cost == bestCost) {
						if (executable.getDeclaringClass() == bestMatch.getDeclaringClass())
							throw new RuntimeException("I " + (name.equals("new") ? "costruttori" : "metodi") + " " + bestMatch + " and " + executable + " has equal cost: " + bestCost);
						if (executable.getDeclaringClass().isAssignableFrom(bestMatch.getDeclaringClass())) continue;
					}
					bestCost = cost;
					bestMatch = executable;
				}
				if (bestMatch != null) return (T) bestMatch;
				throw new RuntimeException((name.equals("new") ? "Costruttore" : "Metodo") + " non univoco." + executables);
			}
		}
	}

	private static String toString(String name, Class <?> ... argumentsClass) {
		String s = stream(argumentsClass).map(c-> c == null ? null : c.toString()).toList().toString();
		return name + "(" + s.substring(1, s.length()-1) + ")";
	}
	
	private static void addExecutable(
		List<Executable> executables, Class<?> classe, boolean publicMember, boolean constructors,	String name, Class<?>... argumentsClass
	) {
		if (trace) out.println("\tc: " + classe);
		try {
			var t = getExecutor(classe, constructors, publicMember, name, argumentsClass);
			if (trace) out.println("\t\t1: " + t);
			executables.add(t);
		}
		catch (NoSuchMethodException nsme) {
			for (Executable executor: getExecutors(classe, constructors, publicMember)) {
				if (!constructors && !executor.getName().equals(name)) continue;
				if (trace) out.println("\t\tn: " + executor);
				if (argumentsClass != null && argumentsClass.length > 0) {
					boolean isVarArgs = executor.isVarArgs();
					Class [] parametersClass = executor.getParameterTypes();
					if (!isVarArgs && argumentsClass.length != parametersClass.length || argumentsClass.length < parametersClass.length - 1) continue;
					if (!isInvokeConvertible(isVarArgs, parametersClass, argumentsClass)) continue;
				}
				executables.add(executor);
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
				// but an interface match should override anything where we have to get a superclass.
				cost += 0.25f;
				break;
			}
			cost++;
			srcClass = srcClass.getSuperclass();
		}
		// If the destination class is null, we've traveled all the way up to an Object match.
		// We'll penalize this by adding 1.5 to the cost.
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
	
	static public String read() throws Exception {
		return read(0);
	}
	
	static InputStreamReader isr = new InputStreamReader(in, forName("UTF-8"));
	
	static public String read(int lv) throws Exception {
		return read(lv, isr);
	}
	static public String read(int lv, Reader in) throws IOException {
		return read(lv, in, false);
	}
	static public String read(int lv, Reader in, boolean inMlComment) throws IOException {
		var s = new StringBuilder();
		int open = 0, close = 0;
		boolean inEscape = false, inChar=false, inSymbol1=false, inString = false, inSymbol = false, inUSymbol = false, inComment=false, sMlComment=false, eMlComment=false;
		do {
			//out.println("loop");
			var oc = close-open;
			if (!in.ready()) out.print((lv == 0 ? "" : lv + "|") + (oc==0 ? "> " : "%+d%s> ".formatted(oc, oc>0 ? "(" : ")")));
			for (int c; (c = in.read()) != '\n' || inString || inUSymbol || inMlComment;) {
				//out.printf("%d %c \n",c, c==-1?' ':c);
				if (c == -1) { out.println("ctrl-z"); return ""; }
				if (inEscape) {
					inEscape = false;
				}
				else if (inChar) {
					inChar = false;
				}
				else if (inSymbol1) {
					inSymbol1 = false;
				}
				else if (inString) switch (c) {
					case '\\'-> inEscape = true;
					case '"'-> inString = false;
				}
				else if (inSymbol) switch (c) {
					case '('-> { inSymbol = false; open += 1; }
					case ')'-> { inSymbol = false; close += 1; }
					default-> {
						if (c <= 32) inSymbol = false;
					}
				}
				else if (inUSymbol) switch (c) {
					case '\\'-> inEscape = true;
					case '|'-> inUSymbol = false;
					/*default-> {
						if (c <= 32) inUSymbol = false;
					}*/
				}
				else if (inComment) switch (c) {
					case '"'-> inString = true;
					case '\n'-> inComment = false;
				}
				else if (eMlComment) switch (c) {
					case '#'-> { return s.toString(); }
					// ignoro gli USymbol nei commenti multilinea
					//default -> inUSymbol = !(eMlComment = false);
					default -> eMlComment = false;
				}
				else if (inMlComment) switch (c) {
					case '"'-> inString = true;
					case ';'-> inComment = true;
					case '|'-> eMlComment = true;
					case '#'-> sMlComment = true;
				}
				else if (sMlComment) switch (c) {
					case '|'-> {
						s.append((char) c);
						s.append(read(lv, in, !(sMlComment = false)));
						c = '#';
					}
					case '\\'-> {
						inChar = true;
						sMlComment = false;
					}
					case 's'-> {
						inSymbol1 = true;
						sMlComment = false;
					}
					default -> sMlComment = false;
				}
				else switch (c) {
					case '"'-> inString = true;
					case '|'-> inUSymbol = true;
					case ';'-> inComment = true;
					case '#'-> sMlComment = true;
					case '('-> open += 1;
					case ')'-> close += 1; 
					default-> {
						if (c > 32) inSymbol = true;
					}
				}
				if (c >= 32 || inUSymbol) s.append((char) c);
			}
			if (inComment) { inComment = false; }
			s.append('\n'); 
		} while (open > close);
		return s.toString();
	}
}
