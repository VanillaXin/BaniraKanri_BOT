package xin.vanilla.banira.util;

import jakarta.annotation.Nullable;
import lombok.extern.slf4j.Slf4j;

import java.lang.reflect.Field;
import java.util.Optional;

@Slf4j
public class ReflectionUtils {

    /**
     * 递归向上查找字段（包括父类）
     */
    public static Optional<Field> findField(Class<?> clazz, String fieldName) {
        Class<?> current = clazz;
        while (current != null && current != Object.class) {
            try {
                Field f = current.getDeclaredField(fieldName);
                f.setAccessible(true);
                return Optional.of(f);
            } catch (NoSuchFieldException ignored) {
            }
            current = current.getSuperclass();
        }
        return Optional.empty();
    }

    /**
     * 读取对象私有字段的值（若存在）
     */
    public static @Nullable <T> T getFieldValue(Object target, String fieldName, Class<T> castTo) {
        if (target == null) return null;
        return findField(target.getClass(), fieldName)
                .map(f -> {
                    try {
                        Object val = f.get(target);
                        return castTo.cast(val);
                    } catch (IllegalAccessException e) {
                        LOGGER.error("Failed to get field {}", fieldName, e);
                        return null;
                    }
                })
                .orElse(null);
    }

    /**
     * 设置对象私有字段的值（若字段存在）
     */
    public static boolean setFieldValue(Object target, String fieldName, Object value) {
        if (target == null) return false;
        Optional<Field> opt = findField(target.getClass(), fieldName);
        if (opt.isEmpty()) return false;
        try {
            Field field = opt.get();
            field.set(target, value);
            return true;
        } catch (IllegalAccessException e) {
            LOGGER.error("Failed to set field {}", fieldName, e);
            return false;
        }
    }
}
