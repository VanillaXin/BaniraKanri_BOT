package xin.vanilla.banira.util.lambda;

import java.beans.Introspector;
import java.lang.invoke.SerializedLambda;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;

/**
 * lambda工具类
 */
public class LambdaUtils {

    /**
     * 传入lambda表达式获取其字段名称
     */
    public static <T> String getFiledName(SerializedFunction<T, ?> sFunction) {
        try {
            Method method = sFunction.getClass().getDeclaredMethod("writeReplace");
            method.setAccessible(Boolean.TRUE);
            SerializedLambda serializedLambda = (SerializedLambda) method.invoke(sFunction);
            String getterMethod = serializedLambda.getImplMethodName();
            return Introspector.decapitalize(methodToProperty(getterMethod));
        } catch (NoSuchMethodException | IllegalAccessException | InvocationTargetException e) {
            throw new IllegalArgumentException("Cannot get filedName by function", e);
        }
    }

    /**
     * 通过readMethod名称获取字段名称
     */
    public static String methodToProperty(String fieldName) {
        if (fieldName.startsWith("is")) {
            fieldName = fieldName.substring(2);
        } else if (fieldName.startsWith("get") || fieldName.startsWith("set")) {
            fieldName = fieldName.substring(3);
        } else {
            throw new IllegalArgumentException("Error parsing property name '" + fieldName + "'.  Didn't start with 'is', 'get' or 'set'.");
        }

        if (fieldName.length() == 1 || (fieldName.length() > 1 && !Character.isUpperCase(fieldName.charAt(1)))) {
            fieldName = fieldName.substring(0, 1).toLowerCase(Locale.ENGLISH) + fieldName.substring(1);
        }

        return fieldName;
    }
}
