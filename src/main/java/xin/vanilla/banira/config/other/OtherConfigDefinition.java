package xin.vanilla.banira.config.other;

import java.util.Objects;
import java.util.function.Supplier;

public final class OtherConfigDefinition<T> {
    private final String key;
    private final OtherConfigScope scope;
    private final Class<T> type;
    private final Supplier<T> defaultSupplier;

    private OtherConfigDefinition(String key, OtherConfigScope scope, Class<T> type, Supplier<T> defaultSupplier) {
        this.key = key;
        this.scope = scope;
        this.type = type;
        this.defaultSupplier = defaultSupplier;
    }

    public static <T> OtherConfigDefinition<T> shared(String key, Class<T> type, Supplier<T> defaultSupplier) {
        return new OtherConfigDefinition<>(key, OtherConfigScope.SHARED, type, defaultSupplier);
    }

    public static <T> OtherConfigDefinition<T> grouped(String key, Class<T> type, Supplier<T> defaultSupplier) {
        return new OtherConfigDefinition<>(key, OtherConfigScope.GROUPED, type, defaultSupplier);
    }

    public String key() {
        return key;
    }

    public OtherConfigScope scope() {
        return scope;
    }

    public Class<T> type() {
        return type;
    }

    public Supplier<T> defaultSupplier() {
        return defaultSupplier;
    }

    public void validate() {
        Objects.requireNonNull(key, "other config key is required");
        Objects.requireNonNull(scope, "other config scope is required");
        Objects.requireNonNull(type, "other config type is required");
        Objects.requireNonNull(defaultSupplier, "other config default supplier is required");
        if (key.isBlank()) {
            throw new IllegalArgumentException("other config key must not be blank");
        }
    }
}
