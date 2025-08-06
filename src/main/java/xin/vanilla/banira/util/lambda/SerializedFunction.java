package xin.vanilla.banira.util.lambda;

import java.io.Serializable;
import java.util.function.Function;

/**
 * 序列化的function函数式接口
 */
@FunctionalInterface
public interface SerializedFunction<T, R> extends Function<T, R>, Serializable {
}
