package com.mikuac.shiro.common.utils;

import com.mikuac.shiro.exception.ShiroException;
import tools.jackson.databind.JsonNode;
import tools.jackson.databind.ObjectMapper;
import tools.jackson.databind.node.ObjectNode;

import java.util.HashMap;
import java.util.Map;
import java.util.Optional;

public class JsonObjectWrapper {
    private static final ObjectMapper OBJECT_MAPPER = JsonUtils.getObjectMapper();
    private static final Map<Class<?>, ValueSetter> VALUE_SETTERS = new HashMap<>();

    static {
        VALUE_SETTERS.put(String.class, (node, key, value) -> node.put(key, (String) value));
        VALUE_SETTERS.put(Integer.class, (node, key, value) -> node.put(key, (Integer) value));
        VALUE_SETTERS.put(Long.class, (node, key, value) -> node.put(key, (Long) value));
        VALUE_SETTERS.put(Boolean.class, (node, key, value) -> node.put(key, (Boolean) value));
        VALUE_SETTERS.put(Double.class, (node, key, value) -> node.put(key, (Double) value));
        VALUE_SETTERS.put(Float.class, (node, key, value) -> node.put(key, (Float) value));
    }

    private final ObjectNode objectNode;

    public JsonObjectWrapper() {
        this.objectNode = OBJECT_MAPPER.createObjectNode();
    }

    public JsonObjectWrapper(ObjectNode objectNode) {
        this.objectNode = objectNode != null ? objectNode : OBJECT_MAPPER.createObjectNode();
    }

    public JsonObjectWrapper(String json) {
        Optional<JsonNode> optionalNode = JsonUtils.parseObject(json);
        this.objectNode = optionalNode
                .filter(ObjectNode.class::isInstance)
                .map(ObjectNode.class::cast)
                .orElseGet(OBJECT_MAPPER::createObjectNode);
    }

    public static JsonObjectWrapper parseObject(String json) {
        return new JsonObjectWrapper(json);
    }

    public JsonObjectWrapper put(String key, Object value) {
        if (value == null) {
            objectNode.putNull(key);
            return this;
        }

        ValueSetter valueSetter = VALUE_SETTERS.get(value.getClass());
        if (valueSetter != null) {
            valueSetter.set(objectNode, key, value);
        } else {
            objectNode.set(key, OBJECT_MAPPER.valueToTree(value));
        }
        return this;
    }

    public Object get(String key) {
        JsonNode jsonNode = objectNode.get(key);
        if (jsonNode == null || jsonNode.isNull()) {
            return null;
        }
        return switch (jsonNode.getNodeType()) {
            case STRING -> jsonNode.asString();
            case NUMBER -> {
                if (jsonNode.isInt()) {
                    yield jsonNode.asInt();
                }
                if (jsonNode.isLong()) {
                    yield jsonNode.asLong();
                }
                yield jsonNode.asDouble();
            }
            case BOOLEAN -> jsonNode.asBoolean();
            default -> jsonNode;
        };
    }

    public String getString(String key) {
        JsonNode jsonNode = objectNode.get(key);
        if (jsonNode != null && jsonNode.isString()) {
            return jsonNode.asString();
        }
        return null;
    }

    public Integer getInt(String key) {
        JsonNode jsonNode = objectNode.get(key);
        if (jsonNode != null && jsonNode.isInt()) {
            return jsonNode.asInt();
        }
        return null;
    }

    public Long getLong(String key) {
        JsonNode jsonNode = objectNode.get(key);
        if (jsonNode != null && jsonNode.isLong()) {
            return jsonNode.asLong();
        }
        return null;
    }

    public Boolean getBoolean(String key) {
        JsonNode jsonNode = objectNode.get(key);
        if (jsonNode != null && jsonNode.isBoolean()) {
            return jsonNode.asBoolean();
        }
        return null;
    }

    public boolean containsKey(String key) {
        return objectNode.has(key);
    }

    public Object getOrDefault(String key, Object defaultValue) {
        Object result = get(key);
        return result != null ? result : defaultValue;
    }

    public JsonObjectWrapper remove(String key) {
        objectNode.remove(key);
        return this;
    }

    public void clear() {
        objectNode.removeAll();
    }

    public int size() {
        return objectNode.size();
    }

    public <T> T to(Class<T> clazz) {
        try {
            return OBJECT_MAPPER.treeToValue((tools.jackson.core.TreeNode) objectNode, clazz);
        } catch (Exception e) {
            throw new ShiroException("JSON deserialization failed, target class: " + clazz.getName(), e);
        }
    }

    public String toJSONString() {
        try {
            return OBJECT_MAPPER.writeValueAsString(objectNode);
        } catch (Exception e) {
            throw new ShiroException("JSON serialization failed", e);
        }
    }

    @Override
    public String toString() {
        return toJSONString();
    }

    public ObjectNode raw() {
        return objectNode;
    }

    @FunctionalInterface
    private interface ValueSetter {
        void set(ObjectNode node, String key, Object value);
    }
}
